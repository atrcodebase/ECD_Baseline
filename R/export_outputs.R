# Exporting unsterilized version of clean datasets -----------------------------
# Clean not sterilized
write.xlsx(clean_data, paste0(output_data_path, "cleaned_dfs/not sterilized/Clean/ECD_EPL_Baseline_clean_data_", Sys.Date(),".xlsx"))

# Raw not sterilized
write.xlsx(raw_data, paste0(output_data_path, "cleaned_dfs/not sterilized/Raw/ECD_EPL_Baseline_clean_data_", Sys.Date(),".xlsx"))


# Export other outputs ---------------------------------------------------------
# export issues in data sets
if (nrow(correction_log_discrep) > 0){
  
  write.xlsx(correction_log_discrep, paste0(output_data_path, "issues/correction_log_issues/correction_log_issues_", Sys.Date(),".xlsx"))
}

if (nrow(repeat_sheet_issues) > 0){
  write.xlsx(repeat_sheet_issues, paste0(output_data_path, "issues/repeat_sheet_issues/repeat_sheet_issues_", Sys.Date(),".xlsx"))
}

sm_question_issues_dfs = list(
  # "NA issues in select_multiple" = sm_na_issues,
  "parent_series_inconsistencies" = sm_check_result
)

if (nrow(sm_check_result) > 0){
  write.xlsx(sm_question_issues_dfs, paste0(output_data_path, "issues/sm_issues/sm_check_result_", Sys.Date(),".xlsx"))
}


if (nrow(wrong_choices_result) > 0){
  write.xlsx(wrong_choices_result, paste0(output_data_path, "issues/wrong_choices/wrong_choices_result_", Sys.Date(),".xlsx"))
}

# if (nrow(missing_translations) > 0){
#   write.xlsx(missing_translations, paste0(output_data_path, "issues/translation_missing/missing_translations_", Sys.Date(),".xlsx"))
# }

if (nrow(Logic_check_result) > 0){
   write.xlsx(Logic_check_result, paste0(output_data_path, "issues/logic_checks/logic_checks_", Sys.Date(),".xlsx"))
}


if (nrow(calculate_issues) > 0){
  write.xlsx(calculate_issues, paste0(output_data_path, "issues/calculate_issues/calculate_issues_", Sys.Date(),".xlsx"))
}

if (nrow(outlier_check_result) > 0){
  write.xlsx(outlier_check_result, paste0(output_data_path, "issues/outliers/outlier_check_result_", Sys.Date(),".xlsx"))
}

# if (nrow(missing_qa_log) > 0){
#   write.xlsx(missing_qa_log, paste0(output_data_path, "issues/missing_qa/Missing_qa_log_", Sys.Date(),".xlsx"))
# }
# 
# if (nrow(relevancy_issues) > 0){
#   write.xlsx(relevancy_issues, paste0(output_data_path, "issues/relevancy_checks/relevancy_issues_", Sys.Date(),".xlsx"))
# }

