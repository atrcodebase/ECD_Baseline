source("R/functions/check_sm_so_questions_value.R")

to_be_excluded_questions = c("Province_filter", "Surveyor_Name", "Site_Visit_ID","resp_province","district")

wrong_choices_result <- data.frame()
#
for (sheet in names(clean_data)) {
  wrong_choices_result <- rbind(wrong_choices_result,
                                check_so_sm_questions(
                                  df = clean_data[[sheet]] %>% select(!any_of(to_be_excluded_questions)),
                                  kobotool.survey = kobo_tool$survey,
                                  kobotool.choices = kobo_tool$choices
                                ) %>% mutate(Sheet = sheet)
  )
}



names(wrong_choices_result)[names(wrong_choices_result) == "invalid_opts"] = "old_value"


# removing extra elements from the environment
rm(list = c("check_so_sm_questions"))
