# EddyPro Flux Data Processing System

## Overview

This repository contains an R workflow for processing EddyPro datasets (2016–2025).

The workflow automatically:

- scans monthly EddyPro output folders
- detects file headers
- removes unit definition rows
- removes invalid (-9999) rows
- aligns variables by column names
- fills missing variables with NA
- merges full_output and biomet datasets
- performs QA/QC validation

---

## Scripts

### 01_EddyPro_SAFE_MERGE_FINAL_CLEAN.R

Main data processing workflow.

Functions include:

- automatic folder scanning
- monthly data merging
- timestamp generation
- duplicate removal
- variable alignment
- final merged dataset generation

Output:

- full_output_2016_2025_FINAL_SAFE.csv
- biomet_2016_2025_FINAL_SAFE.csv
- fulloutput_biomet_2016_2025_MERGED_clean_SAFE.csv

---

### 02_Final_QAQC_Validation.R

Research-grade QA/QC validation.

Checks include:

- duplicate timestamps
- invalid timestamps
- timestamp coverage
- missing variables
- value integrity

Outputs:

- 01_Basic_File_Check.csv
- 02_Timestamp_Coverage_Check.csv
- 03_Value_Integrity_Check.csv
- 04_Final_QAQC_Summary.csv

---

## Current Version

Version: **v1.0**

Status: **QA/QC PASS**

Date: July 2026

---

## Author

Liu Jiaoying

School of Biological Sciences

Universiti Sains Malaysia
