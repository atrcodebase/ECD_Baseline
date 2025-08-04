# create the necessary columns --------------------------------------------
clean_data$rpt_bio_children <- clean_data$rpt_bio_children %>% 
  left_join(
    clean_data$data %>% select(SubmissionDate, Starttime, Endtime, KEY), by = c("PARENT_KEY" = "KEY")
  ) %>% 
  mutate(
    child_years_calc.re_calc = case_when(
      # child_age_date_est == "Yes, enter exact birth date" ~ as.integer(round(((as.Date(Starttime) - as.Date(child_date)) / 365.25),0)),
      child_age_date_est == "Yes, enter exact birth date" ~ as.integer(round(((as.Date(SubmissionDate) - as.Date(child_date)) / 365.25),0)),
      TRUE ~ child_age_est_years
    ),
    
    child_age_est_years_mult_12 = child_age_est_years * 12,
    child_months_calc.re_calc = case_when(
      # child_age_date_est == "Yes, enter exact birth date" ~ as.integer(round(((as.Date(Starttime) - as.Date(child_date)) / 30.43),0)),
      child_age_date_est == "Yes, enter exact birth date" ~ as.integer(round(((as.Date(SubmissionDate) - as.Date(child_date)) / 30.43),0)),
      TRUE ~ rowSums(cbind(child_age_est_years_mult_12, child_age_est_months))
    ),
    
    child_months_calc_mod.re_calc = child_months_calc.re_calc %% 12
    
  )

# clean_data$data <- clean_data$data %>% 
#   left_join(
#     clean_data$rpt_bio_children %>% 
#       filter(
#         child_months_calc.re_calc >= 24 & child_months_calc.re_calc < 84) %>% 
#       group_by(KEY = PARENT_KEY) %>% 
#       summarise(
#         eldest_child_age.re_calc = max(child_months_calc.re_calc, na.rm = T)
#         ), by = "KEY") %>% 
#   mutate(
#     eldest_child_age_yrs.re_calc = round(eldest_child_age.re_calc/12, 0)
#   )


# compare the calculated values before and after logs replaced ----------------
calculate_issues <- plyr::rbind.fill(
  clean_data$rpt_bio_children |>
    filter(child_years_calc != child_years_calc.re_calc) |>
    mutate(
      issue = "The changes in the dataset has affected this value, it should be recalculated.",
      question ="child_years_calc",
      sheet = "rpt_bio_children"
    ) |>
    select(any_of(meta_cols), question, old_value = child_years_calc,
           new_value = child_years_calc.re_calc, issue, sheet, KEY),
  
  clean_data$rpt_bio_children |>
    filter(child_months_calc != child_months_calc.re_calc) |>
    mutate(
      issue = "The changes in the dataset has affected this value, it should be recalculated.",
      question ="child_months_calc",
      sheet = "rpt_bio_children"
    ) |>
    select(any_of(meta_cols), question, old_value = child_months_calc,
           new_value = child_months_calc.re_calc, issue, sheet, KEY),
  
  clean_data$rpt_bio_children |>
    filter(child_months_calc_mod != child_months_calc_mod.re_calc) |>
    mutate(
      issue = "The changes in the dataset has affected this value, it should be recalculated.",
      question ="child_months_calc_mod",
      sheet = "rpt_bio_children"
    ) |>
    select(any_of(meta_cols), question, old_value = child_months_calc_mod,
           new_value = child_months_calc_mod.re_calc, issue, sheet, KEY)
  
  # data
  # clean_data$data |>
  #   filter(eldest_child_age != eldest_child_age.re_calc) |>
  #   mutate(
  #     issue = "The changes in the dataset has affected this value, it should be recalculated.",
  #     question ="eldest_child_age",
  #     sheet = "data"
  #   ) |>
  #   select(any_of(meta_cols), question, old_value = eldest_child_age,
  #          new_value = eldest_child_age.re_calc, issue, sheet, KEY),
  # 
  # clean_data$data |>
  #   filter(eldest_child_age_yrs != eldest_child_age_yrs.re_calc) |>
  #   mutate(
  #     issue = "The changes in the dataset has affected this value, it should be recalculated.",
  #     question ="eldest_child_age_yrs",
  #     sheet = "data"
  #   ) |>
  #   select(any_of(meta_cols), question, old_value = eldest_child_age_yrs,
  #          new_value = eldest_child_age_yrs.re_calc, issue, sheet, KEY)
)


# remove extra indicators that were created in above -------------------------------
# clean_data$data <- clean_data$data |> select(-ends_with(".re_calc"))
# clean_data$rpt_bio_children <- clean_data$rpt_bio_children |> select(-ends_with(".re_calc"))
# 