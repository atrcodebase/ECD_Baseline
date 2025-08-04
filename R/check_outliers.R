if(!require(tidyverse)) install.packages("remotes")
if(!require(cleaninginspectoR)) remotes::install_github("ellieallien/cleaninginspectoR", dependencies = T)

# to_be_ignored_vars <- c("D7_Year", "G4_Minutes")
to_be_ignored_vars <- c("D2_Years", "D2_Months", "D7_Year", "D7_Month", "G4_Minutes")

# extracting the numeric indicators of each tool
numeric_vars <- kobo_tool$survey |> filter(type == "integer" & !name %in% to_be_ignored_vars) |> pull(name)


# creating the indices to facilitate joining of meta cols and KEY ---------
for(sheet in names(clean_data)){
  assign(paste0("meta_cols_tool0",".",sheet), clean_data[[sheet]] %>% mutate(index = as.numeric(row.names(.))) %>% select(index, any_of(unname(meta_cols)), KEY))
}


# logging the outliers ---------------------------------------------------- HERE
outlier_check_result <- data.frame()

# Tool 0
outliers <- data.frame()
for(sheet in names(clean_data)){
  if(any(numeric_vars %in% names(clean_data[[sheet]]))){
    outliers <- plyr::rbind.fill(
      outliers,
      find_outliers(clean_data[[sheet]]) %>% 
        left_join(get(paste0("meta_cols_tool0.",sheet)), by = "index") %>% 
        mutate(Tab_name = sheet)
    )
  }
}

outliers <- outliers %>% 
  filter(variable %in% numeric_vars) %>%
  select(KEY,any_of(meta_cols), index, question_name = variable, old_value = value, Tab_name) |> filter(old_value != 8888)


outlier_check_result <- rbind(
  outlier_check_result,
  outliers
)


# removing extra elements from the environment
rm(list = c(
  objects(pattern = "\\.numeric_vars$"),
  objects(pattern = "^meta_cols_tool.*")
  ))
