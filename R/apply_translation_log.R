
check_logs_for_df <- function(cleaning_log, df, deleted_keys) {
  # Log issues in all the sheets
  for(sheet in names(df)){
    question_names <- names(df[[sheet]]) # All Columns
    keys <- df[[sheet]][["KEY"]] # Keys
    
    # Flag if question or key is not in dataset
    tool_sheet_indexes <- (cleaning_log$Tab_Name %in% sheet) # Logs for the current sheet
    cleaning_log$issue[tool_sheet_indexes & cleaning_log$question %notin% question_names] <- "Question can't be found in the given sheet"
    cleaning_log$issue[tool_sheet_indexes & cleaning_log$KEY %notin% keys] <- "KEY can't be found in the given sheet"
  }
  return(cleaning_log)
}

# Reviewing the correction log -------------------------------------------------
# Identify correction log's issue
translation_log_issues <- translation_log |>
  mutate(
    issue = case_when(
      # general checks
      is.na(KEY) | KEY == "" ~ "KEY can't be null, please provide the correct KEY.",
      is.na(question) | question == "" ~ "Question name can't be null, please provide the correct question name.",
      # is.na(tool) | tool == "" ~ "Tool name can't be null, please provide the correct tool name.",
      is.na(Tab_Name) | Tab_Name == "" ~ "Tab/Sheet name can't be null, please provide the correct Tab name.",
      duplicated(paste0(KEY, question), fromLast = T) | duplicated(paste0(KEY, question), fromLast = F) ~ "Duplicate log records, please solve the duplication.",
      TRUE ~ NA_character_
    ),
    # new_value = case_when(
    #   question %in% sm_variables ~ str_replace_all(new_value, "-|,|  | - ", " ") %>% str_squish(), # Unify the separator in SM questions
    #   TRUE ~ str_squish(new_value)
    # )
  ) |> 
  select(KEY, question, old_value, new_value, issue, tool, Tab_Name)

# Log incorrect sheet name and UUIDs
translation_log_issues <- translation_log_issues |> check_logs_for_df(df = raw_data, deleted_keys = deleted_keys)

## Correction Log ready to apply
translation_log_ready <- translation_log_issues |>
  filter(is.na(issue))

## Correction Log issues
translation_log_issues <- translation_log_issues |>
  filter(!is.na(issue)) |>
  arrange(KEY, question)

# Clone Sheets for log apply verification -------------------------------------
clean_data <- raw_data

# Apply logs -------------------------------------------------------------------
# Tool 0
for(sheet in names(clean_data)){
  # Apply Log
  clean_data[[sheet]] <- apply_log(data=clean_data[[sheet]], log = filter(translation_log_ready, Tab_Name == sheet))
}


# Correction Log apply check --------------------------------------------------
message("Verifying Translation log, please wait!")

# Update the compare_df function in atrFunctions
translation_log_discrep <- data.frame()

# names(clean_data$data[,which(duplicated(names(clean_data$data), fromLast = T) | duplicated(names(clean_data$data), fromLast = F))])
# names(clean_data$data[,which(duplicated(names(clean_data$data), fromLast = T) | duplicated(names(clean_data$data), fromLast = F))])

# Tool 0
for(sheet in names(clean_data)){
  # Compare
  translation_log_discrep <- rbind(
    translation_log_discrep,
    compare_dt(clean_data[[sheet]], clean_data[[sheet]]) |>
      mutate(Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, Tab_Name))
  )
}

# Exclude the correction logs
required_cols <- c("KEY", "question", "old_value", "new_value", "Tab_Name")
translation_log_discrep <- translation_log_discrep |> 
  select(all_of(c(required_cols,"KEY_join"))) |>
  anti_join(translation_log_ready |>
              mutate(KEY_join = paste0(KEY, question, new_value, Tab_Name))
            , by = "KEY_join") |>
  mutate(question = as.character(question)) %>% 
  left_join(translation_log_ready |> # Join Sample Type
              select(KEY, question, Tab_Name), by = c("KEY", "question", "Tab_Name")) |> 
  mutate(issue = "The new_value is not applied correctly, plz check if new_value is consistent with the question!") |>
  select(-KEY_join)

# Join with Correction log issues
translation_log_discrep <- rbind( 
  translation_log_discrep,
  translation_log_issues |>
    select(all_of(required_cols), "issue")
)

# Removing extra objects -------------------------------------------------------
rm(list = c("translation_log_issues",
            "translation_log_ready"))
