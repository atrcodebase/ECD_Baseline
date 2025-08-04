# Tool 0 ----------------------------------------------------------------------
for (sheet in names(clean_data)) {
  clean_data[[sheet]] <- update_series_cols(data = clean_data[[sheet]],
                                                  tool = kobo_tool$survey,
                                                  question_separator = "_")
}


# removing extra elements from the environment
rm(list = c(
  objects(pattern = ".sm_vars$")
))



