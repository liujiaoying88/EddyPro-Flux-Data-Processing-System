# ============================================================
# EddyPro SAFE MERGE FINAL - CLEAN VERSION
# 按列名合并，不按列位置
# 删除残留 -9999 说明行
# ============================================================

library(dplyr)

base_path <- "~/Documents"
target_years <- 2016:2025
station_name <- "eddypro_muka_head01"
data_types <- c("full_output", "biomet")

month_map <- c(
  Jan = "01", Feb = "02", Mar = "03", Apr = "04",
  May = "05", Jun = "06", Jul = "07", Aug = "08",
  Sep = "09", Oct = "10", Nov = "11", Dec = "12"
)

get_folder_month <- function(folder_path) {
  folder_name <- basename(folder_path)
  if(!grepl("^[A-Za-z]{3}_[0-9]{4}$", folder_name)) return(NA)
  mon_text <- substr(folder_name, 1, 3)
  year_text <- substr(folder_name, 5, 8)
  if(!mon_text %in% names(month_map)) return(NA)
  paste0(year_text, month_map[[mon_text]])
}

fix_column_names <- function(df) {
  nm <- names(df)
  bad <- is.na(nm) | nm == ""
  if(any(bad)) nm[bad] <- paste0("UNNAMED_", seq_len(sum(bad)))
  names(df) <- make.unique(nm)
  df
}

detect_header_line <- function(lines) {
  idx <- which(
    grepl("filename\\s*,\\s*date\\s*,\\s*time", lines, ignore.case = TRUE) |
      grepl("^\\s*date\\s*,\\s*time", lines, ignore.case = TRUE) |
      grepl("TIMESTAMP_START", lines, ignore.case = TRUE) |
      grepl("^\\s*TIMESTAMP\\s*,", lines, ignore.case = TRUE)
  )
  if(length(idx) == 0) return(NA)
  idx[1]
}

read_eddy_file <- function(file) {
  lines <- readLines(file, warn = FALSE)
  header_line <- detect_header_line(lines)
  if(is.na(header_line)) return(NULL)
  
  df <- read.csv(
    file,
    skip = header_line - 1,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  fix_column_names(df)
}

is_unit_row <- function(df) {
  flag <- rep(FALSE, nrow(df))
  for(col in names(df)) {
    v <- trimws(as.character(df[[col]]))
    flag <- flag | grepl("^\\[.*\\]$", v)
  }
  flag
}

is_missing_definition_row <- function(df) {
  flag <- rep(FALSE, nrow(df))
  if(nrow(df) == 0) return(flag)
  
  first_col <- names(df)[1]
  first_values <- trimws(as.character(df[[first_col]]))
  check_rows <- seq_len(min(10, nrow(df)))
  
  flag[check_rows] <- first_values[check_rows] == "-9999"
  flag
}

make_timestamp <- function(df) {
  nl <- tolower(names(df))
  
  if("date" %in% nl && "time" %in% nl) {
    date_col <- names(df)[which(nl == "date")[1]]
    time_col <- names(df)[which(nl == "time")[1]]
    return(trimws(paste(as.character(df[[date_col]]), as.character(df[[time_col]]))))
  }
  
  if("timestamp_start" %in% nl) {
    col <- names(df)[which(nl == "timestamp_start")[1]]
    return(trimws(as.character(df[[col]])))
  }
  
  if("timestamp" %in% nl) {
    col <- names(df)[which(nl == "timestamp")[1]]
    return(trimws(as.character(df[[col]])))
  }
  
  rep(NA, nrow(df))
}

align_by_names <- function(df, master_cols) {
  missing_cols <- setdiff(master_cols, names(df))
  for(col in missing_cols) df[[col]] <- NA
  df[, master_cols]
}

remove_invalid_timestamp_rows <- function(df) {
  df <- df %>%
    filter(
      !is.na(TIMESTAMP),
      TIMESTAMP != "",
      TIMESTAMP != "NA",
      TIMESTAMP != "-9999",
      !grepl("^\\[.*\\]$", TIMESTAMP)
    )
  df
}

all_folders <- list.dirs(base_path, recursive = TRUE, full.names = TRUE)
folder_months <- sapply(all_folders, get_folder_month)

month_folder_table <- data.frame(
  folder = all_folders,
  month = folder_months,
  stringsAsFactors = FALSE
)

month_folder_table <- month_folder_table[!is.na(month_folder_table$month), ]
month_folder_table <- month_folder_table[
  substr(month_folder_table$month, 1, 4) %in% as.character(target_years),
]

merge_one_type <- function(data_type, prefix) {
  
  cat("\n========================================\n")
  cat("开始合并：", data_type, "\n")
  cat("========================================\n")
  
  file_pattern <- paste0("eddypro_.*", data_type, ".*\\.csv$")
  data_files <- data.frame()
  
  for(i in seq_len(nrow(month_folder_table))) {
    folder <- month_folder_table$folder[i]
    folder_month <- month_folder_table$month[i]
    folder_year <- substr(folder_month, 1, 4)
    
    files <- list.files(
      path = folder,
      pattern = file_pattern,
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    if(length(files) > 0) {
      data_files <- bind_rows(
        data_files,
        data.frame(
          data_type = data_type,
          year = folder_year,
          month = folder_month,
          file = files,
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  data_files <- data_files %>% arrange(month, file)
  cat("找到文件数量：", nrow(data_files), "\n")
  
  data_list <- list()
  master_cols <- character(0)
  merge_log <- data.frame()
  
  for(i in seq_len(nrow(data_files))) {
    
    file <- data_files$file[i]
    file_month <- data_files$month[i]
    file_year <- data_files$year[i]
    
    cat("\n读取：", file_month, " ", basename(file), "\n")
    
    df <- tryCatch(read_eddy_file(file), error = function(e) NULL)
    
    if(is.null(df)) {
      merge_log <- bind_rows(
        merge_log,
        data.frame(data_type, year = file_year, month = file_month,
                   file = basename(file), status = "READ_FAILED",
                   rows = NA, cols = NA)
      )
      next
    }
    
    df <- df[rowSums(!is.na(df) & df != "") > 0, , drop = FALSE]
    if(nrow(df) == 0) next
    
    unit_rows <- is_unit_row(df)
    df <- df[!unit_rows, , drop = FALSE]
    
    missing_def_rows <- is_missing_definition_row(df)
    df <- df[!missing_def_rows, , drop = FALSE]
    
    if(nrow(df) == 0) next
    
    df$TIMESTAMP <- make_timestamp(df)
    df <- remove_invalid_timestamp_rows(df)
    
    if(nrow(df) == 0) {
      merge_log <- bind_rows(
        merge_log,
        data.frame(data_type, year = file_year, month = file_month,
                   file = basename(file), status = "NO_VALID_TIMESTAMP",
                   rows = NA, cols = NA)
      )
      next
    }
    
    df$SOURCE_YEAR <- file_year
    df$SOURCE_MONTH <- file_month
    df$SOURCE_FILE <- basename(file)
    
    df[] <- lapply(df, as.character)
    
    master_cols <- unique(c(master_cols, names(df)))
    data_list[[length(data_list) + 1]] <- df
    
    merge_log <- bind_rows(
      merge_log,
      data.frame(
        data_type = data_type,
        year = file_year,
        month = file_month,
        file = basename(file),
        status = "OK",
        rows = nrow(df),
        cols = ncol(df),
        stringsAsFactors = FALSE
      )
    )
    
    cat("有效行数：", nrow(df), " 列数：", ncol(df), "\n")
  }
  
  if(length(data_list) == 0) {
    stop(paste0(data_type, " 没有有效数据"))
  }
  
  source_cols <- c("SOURCE_YEAR", "SOURCE_MONTH", "SOURCE_FILE")
  master_cols <- c(
    "TIMESTAMP",
    setdiff(master_cols, c("TIMESTAMP", source_cols)),
    source_cols
  )
  
  data_list_aligned <- lapply(data_list, align_by_names, master_cols = master_cols)
  merged_data <- bind_rows(data_list_aligned)
  
  merged_data <- remove_invalid_timestamp_rows(merged_data)
  
  duplicate_rows <- merged_data[
    duplicated(merged_data$TIMESTAMP) |
      duplicated(merged_data$TIMESTAMP, fromLast = TRUE),
    ,
    drop = FALSE
  ]
  
  merged_data <- merged_data %>%
    arrange(TIMESTAMP) %>%
    distinct(TIMESTAMP, .keep_all = TRUE)
  
  output_file <- file.path(
    base_path,
    paste0(station_name, "_", data_type, "_2016_2025_FINAL_SAFE.csv")
  )
  
  write.csv(merged_data, output_file, row.names = FALSE, na = "NA")
  
  write.csv(
    merge_log,
    file.path(base_path, paste0(station_name, "_", data_type, "_merge_log_SAFE.csv")),
    row.names = FALSE,
    na = "NA"
  )
  
  write.csv(
    duplicate_rows,
    file.path(base_path, paste0(station_name, "_", data_type, "_duplicate_timestamp_SAFE.csv")),
    row.names = FALSE,
    na = "NA"
  )
  
  key_cols <- c("TIMESTAMP")
  source_cols <- c("SOURCE_YEAR", "SOURCE_MONTH", "SOURCE_FILE")
  value_cols <- setdiff(names(merged_data), c(key_cols, source_cols))
  
  names(merged_data)[names(merged_data) %in% value_cols] <- paste0(prefix, value_cols)
  names(merged_data)[names(merged_data) == "SOURCE_YEAR"] <- paste0(prefix, "SOURCE_YEAR")
  names(merged_data)[names(merged_data) == "SOURCE_MONTH"] <- paste0(prefix, "SOURCE_MONTH")
  names(merged_data)[names(merged_data) == "SOURCE_FILE"] <- paste0(prefix, "SOURCE_FILE")
  
  cat("\n完成：", data_type, "\n")
  cat("最终行数：", nrow(merged_data), "\n")
  cat("最终列数：", ncol(merged_data), "\n")
  
  return(merged_data)
}

full_output_safe <- merge_one_type(
  data_type = "full_output",
  prefix = "FULL_"
)

biomet_safe <- merge_one_type(
  data_type = "biomet",
  prefix = "BIO_"
)

final_merged <- full_output_safe %>%
  full_join(
    biomet_safe,
    by = "TIMESTAMP"
  ) %>%
  arrange(TIMESTAMP)

final_merged <- remove_invalid_timestamp_rows(final_merged)

final_merged$DATE_FINAL <- substr(final_merged$TIMESTAMP, 1, 10)
final_merged$TIME_FINAL <- substr(final_merged$TIMESTAMP, 12, 16)

front_cols <- c("TIMESTAMP", "DATE_FINAL", "TIME_FINAL")

final_merged <- final_merged[
  ,
  c(front_cols, setdiff(names(final_merged), front_cols))
]

final_output_file <- file.path(
  base_path,
  paste0(station_name, "_fulloutput_biomet_2016_2025_MERGED_clean_SAFE.csv")
)

write.csv(
  final_merged,
  final_output_file,
  row.names = FALSE,
  na = "NA"
)

cat("\n========================================\n")
cat("全部完成\n")
cat("========================================\n")
cat("最终主分析文件：\n")
cat(final_output_file, "\n")
