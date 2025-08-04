source("R/functions/check_select_multiple.R")

sm_check_result <- data.frame()

for(sheet in names(clean_data)){
  sm_check_result <- plyr::rbind.fill(
    sm_check_result,
    check_select_multiple(
      data = clean_data[[sheet]],
      tool = kobo_tool$survey,
      question_separator = "_"
    ) %>% mutate(Tab_name = sheet)
  )
}

# removing extra elements from the environment
rm(list = c(
  objects(pattern = "^multi_vars_")
))



