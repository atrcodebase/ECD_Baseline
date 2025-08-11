# Organization:   ATR
# Date:           August 2025
# Script:         WB-ECD (Baseline)
# Author:         ATR Data Management Department

# Load required packages -------------------------------------------------------
rm(list = ls())
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(googlesheets4)) install.packages("googlesheets4")
if(!require(openxlsx)) install.packages("openxlsx")
if(!require(readxl)) install.packages("readxl")
if(!require(readxl)) install.packages("lubridate")
if(!require(readxl)) install.packages("janitor")
if(!require(remotes)) install.packages("remotes")
if(!require(plyr)) install.packages("plyr")
if(!require(atRfunctions)) remotes::install_github("atrcodebase/atRfunctions")
`%notin%` <- Negate(`%in%`)
Coalesce = function(var) ifelse(is.na(var), 0, var)

# Declaring Global Variables ---------------------------------------------------
qa_sheet_url_ps = "https://docs.google.com/spreadsheets/d/1XzjVn_kNJEbzrxjjFB96ZI0ryY1ywk2OB9VP5EN7ybg/edit?gid=169480259#gid=169480259"

output_data_path = "output/"

raw_data_path = list.files("input/raw_data/", full.names = T) %>% .[!grepl("/\\~\\$", .)] |> as.list() %>% setNames("Caregiver_tool")
kobo_tools_path = list.files("input/tool/", full.names = T) %>% .[!grepl("/\\~\\$", .)] |> as.list() %>% setNames("Caregiver_tool")
# relevancy_files_path = list.files("input/relevancy_file/", full.names = T) %>% .[!grepl("/\\~\\$", .)] |> as.list() %>% setNames("Caregiver_tool")
meta_cols <- c("Site_Visit_ID","Province", "District","Region")
# meta_cols.qa_sheet <- c(Visit_ID = "Site_Visit_ID", "School Code", "Sample_Type", Survey_Date = "SubmissionDate", Region = "Region", "KEY")

# Read inputs ------------------------------------------------------------------
# Data sets
raw_data = read_xlsx_sheets(raw_data_path$Caregiver_tool)

if(names(raw_data$data[,c(37)]) == "review_status"){
  raw_data$data <- raw_data$data[,-37]
}

# Tools
kobo_tool = read_xlsx_sheets(kobo_tools_path$Caregiver_tool)


# read the relevancy files
# relevancy_file = read_xlsx_sheets(relevancy_files_path$Caregiver_tool)


# Read QA log from Google sheet ------------------------------------------------
qa_sheet = read_sheet(qa_sheet_url_ps, sheet = "QA_Log")
# To select the user to authenticate
2

correction_log = read_sheet(qa_sheet_url_ps, sheet = "Correction_Log")

# Turn NULL values to NA for old and new value columns
correction_log <- correction_log %>% 
  mutate(
    New_Value = as.character(New_Value),
    New_Value = case_when(
      is.null(New_Value) | New_Value == "NULL" ~ NA_character_,
      TRUE ~ New_Value
    ),
    old_value = as.character(old_value),
    old_value = case_when(
      is.null(old_value) | old_value == "NULL" ~ NA_character_,
      TRUE ~ old_value
    )
  ) %>% 
  # Exclude if Unique_key, question, new value, and old value combo is NA
  filter(!((is.na(KEY_Unique)|KEY_Unique %in% c("", " ")) & is.na(Question)))

detach("package:dplyr", unload=TRUE)
library(dplyr)

# Align correction log's variables and tool names
correction_log <- correction_log %>%
  mutate(across(everything(), as.character)) |> 
  rename(
    key = "KEY",
    KEY = "KEY_Unique",
    question = "Question",
    new_value = "New_Value",
    tool = "Tool"
  )

deletion_log <- read_sheet(qa_sheet_url_ps, sheet = "To_Be_Removed_From_Dataset")

# Prepare Data sets - Public School --------------------------------------------
qa_sheet <- qa_sheet |>
  rename(
    qa_status = QA_Status,
    KEY = KEY_Unique) |>
  mutate(
    qa_status = toupper(qa_status), 
    qa_status = case_when(
      is.na(qa_status) ~ "PENDING",
      TRUE ~ qa_status
    )
  )

# Extract Approved Interviews
approved_keys = qa_sheet |> 
  filter(qa_status %in% c("APPROVED")) |> 
  pull(KEY) |> unique()

length(approved_keys) == length(which(qa_sheet$qa_status == "APPROVED"))


# Extract deleted KEYs to be removed from data sets
deleted_keys = deletion_log %>% filter(!is.na(KEY_Unique)) |> pull(KEY_Unique) %>% unique()


# Translation log -----------------------------------------------------------
# detailed_check_log <- read_sheet(qa_sheet_url_ps, sheet = "Detailed_Check")

# photo_status <- detailed_check_log %>% 
  # filter(Check_Type %in% c("image", "text") & !is.na(Check_Status)) %>% # `QA Status` == "Approved"
  # filter(!(Check_Status == "Verified" & Check_Type == "text"))

translation_log <- read_sheet(qa_sheet_url_ps, sheet = "Translation_Log")

translation_log <- translation_log %>% 
  mutate(
    New_Value = as.character(New_Value),
    New_Value = case_when(
      is.null(New_Value) | New_Value == "NULL" ~ NA_character_,
      TRUE ~ New_Value
    ),
    Old_Value = as.character(Old_Value),
    Old_Value = case_when(
      is.null(Old_Value) | Old_Value == "NULL" ~ NA_character_,
      TRUE ~ Old_Value
    )
  ) %>% 
  # Exclude if Unique_key, question, new value, and old value combo is NA
  filter(!((is.na(KEY_Unique)|KEY_Unique %in% c("", " ")) & is.na(Question)))

# Align correction log's variables and tool names
translation_log <- translation_log %>%
  mutate(across(everything(), as.character)) |> 
  rename(
    key = "KEY",
    KEY = "KEY_Unique",
    question = "Question",
    new_value = "New_Value",
    old_value = "Old_Value",
    tool = "Tool"
  )

# Apply Photo status log -------------------------------------------------------
if(nrow(translation_log) > 0) { source("R/apply_translation_log.R") }


# Apply correction log ---------------------------------------------------------
if(nrow(correction_log) > 0) { source("R/apply_correction_log.R") }

# Remove the rejected and pilot interviews -------------------------------------
source("R/remove_rejected_interviews.R")

weekly_data <- read_sheet(qa_sheet_url_ps, sheet = "Clean_Dataset_Keys")

# Filter only second week's data
clean_data$data <- clean_data$data %>%
  filter(KEY %in% weekly_data$streamlit_keys_qa_approved)
clean_data$rpt_bio_children <- clean_data$rpt_bio_children %>%
  filter(PARENT_KEY %in% clean_data$data$KEY)

raw_data$data <- raw_data$data %>%
  filter(KEY %in% c(weekly_data$second_delivery_approved_rejected_data, weekly_data$second_delivery_tryouts_26_9))

raw_data$rpt_bio_children <- raw_data$rpt_bio_children %>%
  filter(PARENT_KEY %in% raw_data$data$KEY)

# Merge meta data from main sheet to repeating groups --------------------------
source("R/main_sheet_to_repeat_sheets.R")


# Update select multiple binary variables --------------------------------------
source("R/fix_select_multiple_questions.R") 


# Check repeat sheet count -----------------------------------------------------
source("R/check_repeat_sheet_counts.R")


# missing translations (for QA)-------------------------------------------------
source("R/create_translation_log.R")


# missing qa (for QA)-----------------------------------------------------------
# source("R/missing_qa.R")


# Check select multiple variables ----------------------------------------------
source("R/check_select_multiple_questions.R")


# convert numeric dates to date and time formats ------------------------------- Move to the end
source("R/convert_numbers_to_date_time.R")


# re-calculate the calculated variables and compare any changes not applied - kDR left
source("R/calculate_cols_check.R")


# Outlier Check ----------------------------------------------------------------
source("R/check_outliers.R")


# Relevancy Check --------------------------------------------------------------
# source("R/check_relevancies.R")


# Check the responses with the tool --------------------------------------------
source("R/compare_df_values_with_tool.R")


# attach value labels  ---------------------------------------------------------
source("R/attach_labels.R")


detach("package:cleaninginspectoR", unload=TRUE)
detach("package:dplyr", unload=TRUE)
library(dplyr)

# Logical inconsistencies ------------------------------------------------- Done
source("R/logical_checks.R")


# remove extra columns  --------------------------------------------------------
source("R/remove_extra_columns.R")


# attach labels to calculates cols ---------------------------------------------
# source("R/attach_calculate_label.R")


# change 7777, 8888, 9999 to Labels  -------------------------------------------
# source("R/recode_to_na.R")

# Convert date
clean_data$rpt_bio_children <- clean_data$rpt_bio_children %>% 
  mutate(
    child_date = format(as.Date(child_date), "%m-%d-%Y")
    # child_date = case_when(
    #   !is.na(child_date) ~ as.character(format(as.Date(child_date), "%-m/%-d/%Y")),
    #   TRUE ~ child_date
    # )
  )

# Correction log issues only for weekly data
correction_log_discrep <- correction_log_discrep %>% 
  mutate(
    KEY_Unique = str_sub(KEY, 1, 41)
  ) %>% 
  relocate(KEY_Unique, .after = KEY) %>% 
  filter(KEY_Unique %in% clean_data$data$KEY)

translation_log_discrep <- translation_log_discrep %>% 
  mutate(
    KEY_Unique = str_sub(KEY, 1, 41)
  ) %>% 
  relocate(KEY_Unique, .after = KEY) %>% 
  filter(KEY_Unique %in% clean_data$data$KEY)

# export data sets and issues --------------------------------------------- DONE
source("R/export_outputs.R")

