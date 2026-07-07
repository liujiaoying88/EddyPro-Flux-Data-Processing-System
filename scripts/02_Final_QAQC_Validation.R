# ============================================================
# Final Research Validation Script
# 检查 full_output、biomet、最终 merged 文件是否一致
# 不修改任何数据
# ============================================================

library(dplyr)

base_path <- "~/Documents"
station_name <- "eddypro_muka_head01"

full_file <- file.path(
  base_path,
  paste0(station_name, "_full_output_2016_2025_FINAL_SAFE.csv")
)

bio_file <- file.path(
  base_path,
  paste0(station_name, "_biomet_2016_2025_FINAL_SAFE.csv")
)

merged_file <- file.path(
  base_path,
  paste0(station_name, "_fulloutput_biomet_2016_2025_MERGED_clean_SAFE.csv")
)

output_dir <- file.path(base_path, "Final_QAQC_Report")

if(!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# =========================
# 1. 读取文件
# =========================

full <- read.csv(
  full_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

bio <- read.csv(
  bio_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

merged <- read.csv(
  merged_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# =========================
# 2. 基础检查
# =========================

basic_report <- data.frame(
  file_type = c("full_output", "biomet", "merged"),
  rows = c(nrow(full), nrow(bio), nrow(merged)),
  cols = c(ncol(full), ncol(bio), ncol(merged)),
  duplicate_timestamp = c(
    sum(duplicated(full$TIMESTAMP)),
    sum(duplicated(bio$TIMESTAMP)),
    sum(duplicated(merged$TIMESTAMP))
  ),
  invalid_timestamp = c(
    sum(is.na(full$TIMESTAMP) | full$TIMESTAMP == "" | full$TIMESTAMP == "-9999"),
    sum(is.na(bio$TIMESTAMP) | bio$TIMESTAMP == "" | bio$TIMESTAMP == "-9999"),
    sum(is.na(merged$TIMESTAMP) | merged$TIMESTAMP == "" | merged$TIMESTAMP == "-9999")
  ),
  stringsAsFactors = FALSE
)

# =========================
# 3. 时间一致性检查
# =========================

full_ts <- unique(full$TIMESTAMP)
bio_ts <- unique(bio$TIMESTAMP)
merged_ts <- unique(merged$TIMESTAMP)

timestamp_report <- data.frame(
  item = c(
    "full_timestamps",
    "bio_timestamps",
    "merged_timestamps",
    "full_not_in_merged",
    "bio_not_in_merged",
    "merged_not_in_full_or_bio"
  ),
  count = c(
    length(full_ts),
    length(bio_ts),
    length(merged_ts),
    length(setdiff(full_ts, merged_ts)),
    length(setdiff(bio_ts, merged_ts)),
    length(setdiff(merged_ts, union(full_ts, bio_ts)))
  ),
  stringsAsFactors = FALSE
)

# =========================
# 4. FULL 数据一致性检查
# =========================

full_compare <- full

full_value_cols <- setdiff(
  names(full_compare),
  c("SOURCE_YEAR", "SOURCE_MONTH", "SOURCE_FILE")
)

full_prefixed_cols <- paste0("FULL_", setdiff(full_value_cols, "TIMESTAMP"))

full_missing_cols <- setdiff(full_prefixed_cols, names(merged))

full_diff_report <- data.frame()

if(length(full_missing_cols) == 0) {
  
  full_merged_subset <- merged %>%
    select(TIMESTAMP, all_of(full_prefixed_cols))
  
  names(full_merged_subset) <- c(
    "TIMESTAMP",
    setdiff(full_value_cols, "TIMESTAMP")
  )
  
  full_joined <- full_compare %>%
    select(all_of(full_value_cols)) %>%
    inner_join(
      full_merged_subset,
      by = "TIMESTAMP",
      suffix = c("_original", "_merged")
    )
  
  check_cols <- setdiff(full_value_cols, "TIMESTAMP")
  
  for(col in check_cols) {
    
    original_col <- paste0(col, "_original")
    merged_col <- paste0(col, "_merged")
    
    if(original_col %in% names(full_joined) && merged_col %in% names(full_joined)) {
      
      diff_count <- sum(
        !(is.na(full_joined[[original_col]]) & is.na(full_joined[[merged_col]])) &
          as.character(full_joined[[original_col]]) != as.character(full_joined[[merged_col]])
      )
      
      full_diff_report <- bind_rows(
        full_diff_report,
        data.frame(
          data_type = "full_output",
          variable = col,
          difference_count = diff_count,
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
} else {
  
  full_diff_report <- data.frame(
    data_type = "full_output",
    variable = full_missing_cols,
    difference_count = NA,
    note = "Column missing in merged file",
    stringsAsFactors = FALSE
  )
}

# =========================
# 5. BIO 数据一致性检查
# =========================

bio_compare <- bio

bio_value_cols <- setdiff(
  names(bio_compare),
  c("SOURCE_YEAR", "SOURCE_MONTH", "SOURCE_FILE")
)

bio_prefixed_cols <- paste0("BIO_", setdiff(bio_value_cols, "TIMESTAMP"))

bio_missing_cols <- setdiff(bio_prefixed_cols, names(merged))

bio_diff_report <- data.frame()

if(length(bio_missing_cols) == 0) {
  
  bio_merged_subset <- merged %>%
    select(TIMESTAMP, all_of(bio_prefixed_cols))
  
  names(bio_merged_subset) <- c(
    "TIMESTAMP",
    setdiff(bio_value_cols, "TIMESTAMP")
  )
  
  bio_joined <- bio_compare %>%
    select(all_of(bio_value_cols)) %>%
    inner_join(
      bio_merged_subset,
      by = "TIMESTAMP",
      suffix = c("_original", "_merged")
    )
  
  check_cols <- setdiff(bio_value_cols, "TIMESTAMP")
  
  for(col in check_cols) {
    
    original_col <- paste0(col, "_original")
    merged_col <- paste0(col, "_merged")
    
    if(original_col %in% names(bio_joined) && merged_col %in% names(bio_joined)) {
      
      diff_count <- sum(
        !(is.na(bio_joined[[original_col]]) & is.na(bio_joined[[merged_col]])) &
          as.character(bio_joined[[original_col]]) != as.character(bio_joined[[merged_col]])
      )
      
      bio_diff_report <- bind_rows(
        bio_diff_report,
        data.frame(
          data_type = "biomet",
          variable = col,
          difference_count = diff_count,
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
} else {
  
  bio_diff_report <- data.frame(
    data_type = "biomet",
    variable = bio_missing_cols,
    difference_count = NA,
    note = "Column missing in merged file",
    stringsAsFactors = FALSE
  )
}

# =========================
# 6. 汇总差异
# =========================

diff_report <- bind_rows(
  full_diff_report,
  bio_diff_report
)

total_difference <- sum(
  diff_report$difference_count,
  na.rm = TRUE
)

missing_column_count <- sum(
  is.na(diff_report$difference_count)
)

# =========================
# 7. 最终结论
# =========================

overall_status <- ifelse(
  sum(basic_report$duplicate_timestamp, na.rm = TRUE) == 0 &&
    sum(basic_report$invalid_timestamp, na.rm = TRUE) == 0 &&
    timestamp_report$count[timestamp_report$item == "full_not_in_merged"] == 0 &&
    timestamp_report$count[timestamp_report$item == "bio_not_in_merged"] == 0 &&
    timestamp_report$count[timestamp_report$item == "merged_not_in_full_or_bio"] == 0 &&
    total_difference == 0 &&
    missing_column_count == 0,
  "PASS",
  "CHECK_REQUIRED"
)

final_summary <- data.frame(
  check_item = c(
    "Duplicate Timestamp",
    "Invalid Timestamp",
    "Timestamp Coverage",
    "Missing Columns",
    "Value Difference",
    "Overall_Status"
  ),
  result = c(
    ifelse(sum(basic_report$duplicate_timestamp) == 0, "PASS", "CHECK_REQUIRED"),
    ifelse(sum(basic_report$invalid_timestamp) == 0, "PASS", "CHECK_REQUIRED"),
    ifelse(
      timestamp_report$count[timestamp_report$item == "full_not_in_merged"] == 0 &&
        timestamp_report$count[timestamp_report$item == "bio_not_in_merged"] == 0 &&
        timestamp_report$count[timestamp_report$item == "merged_not_in_full_or_bio"] == 0,
      "PASS",
      "CHECK_REQUIRED"
    ),
    ifelse(missing_column_count == 0, "PASS", "CHECK_REQUIRED"),
    ifelse(total_difference == 0, "PASS", "CHECK_REQUIRED"),
    overall_status
  ),
  stringsAsFactors = FALSE
)

# =========================
# 8. 输出报告
# =========================

write.csv(
  basic_report,
  file.path(output_dir, "01_Basic_File_Check.csv"),
  row.names = FALSE,
  na = "NA"
)

write.csv(
  timestamp_report,
  file.path(output_dir, "02_Timestamp_Coverage_Check.csv"),
  row.names = FALSE,
  na = "NA"
)

write.csv(
  diff_report,
  file.path(output_dir, "03_Value_Integrity_Check.csv"),
  row.names = FALSE,
  na = "NA"
)

write.csv(
  final_summary,
  file.path(output_dir, "04_Final_QAQC_Summary.csv"),
  row.names = FALSE,
  na = "NA"
)

cat("\n========================================\n")
cat("Final QA/QC Validation Completed\n")
cat("========================================\n\n")

cat("Overall Status: ", overall_status, "\n\n")

cat("Reports saved in:\n")
cat(output_dir, "\n\n")

cat("Please check:\n")
cat("04_Final_QAQC_Summary.csv\n")
cat("03_Value_Integrity_Check.csv\n")
