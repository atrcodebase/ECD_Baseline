library(glue)
# LOGIC CHECK
fruits_meat <- clean_data$data %>% 
  filter(did_you_worry_about_not_having_enough_food == "Yes" & (fruits %in% c("Nearly every day", "More than half the days")
                                                                & meat %in% c("Nearly every day","More than half the days"))) 

lc_data <- plyr::rbind.fill(
  # Flag if the answered No for are_you_primary_main_holder_of_this_phone_number and then answered Myself for 'Who this phone number belongs to?'
  clean_data$data %>% 
    filter(are_you_primary_main_holder_of_this_phone_number == "No" & phone_number_belongs == "Myself (Respondent)") %>% 
    mutate(
      Issue = "The phone is belonged to the respondent, but also answered 'No' for question 'are_you_primary_main_holder_of_this_phone_number'!",
      Question = "are_you_primary_main_holder_of_this_phone_number",
      Old_value = as.character(are_you_primary_main_holder_of_this_phone_number),
      Related_question = "phone_number_belongs",
      Related_value = phone_number_belongs
    ) |> 
    select(
      all_of(meta_cols),
      Question,
      Old_value,
      Related_question,
      Related_value,
      KEY,
      Issue
    ),
  
  # Flagging if the number of HH mem aged 0-12 yrs old is reported greater or equal to the total HH mem
  clean_data$data %>% 
    filter(how_many_children_adolescents >= how_many_people_currently_live_in_your_household) %>% 
    mutate(
      Issue = "The number of HH members aged 0-12 yrs old is reported greater or equal(Respondent didn't count her/himself) to the number of HH members!",
      Question = "how_many_children_adolescents",
      Old_value = as.character(how_many_children_adolescents),
      Related_question = "how_many_people_currently_live_in_your_household",
      Related_value = how_many_people_currently_live_in_your_household
    ) |> 
    select(
      all_of(meta_cols),
      Question,
      Old_value,
      Related_question,
      Related_value,
      KEY,
      Issue
    ),
  
  # Flagging if the child is 5 or 6 and reported s/he is currently enrolled at school (Primary level), but can't communicate (communication_chid_aged_5_6)
  
  
  # Flagging if it is reported 'No activity done' for playing during a week but also reported the number of days for it
  clean_data$data %>% 
    filter(played_by_whom == "No activity done" & (!is.na(played_days) | played_days != 0)) %>% 
    mutate(
      Issue = "It is reported 'No activity done' for playing during a week but also reported the number of days for it!",
      Question = "played_by_whom",
      Old_value = as.character(played_by_whom),
      Related_question = "played_days",
      Related_value = played_days
    ) |> 
    select(
      all_of(meta_cols),
      Question,
      Old_value,
      Related_question,
      Related_value,
      KEY,
      Issue
    ),
  
  # Flagging if Not at all is selected for all food items
  clean_data$data %>% 
    filter(fruits == "Not at all" & vegetable == "Not at all" & meat == "Not at all" &
             dairy_products == "Not at all" & grain_and_legumes == "Not at all") %>% 
    mutate(
      Issue = "The respondent reported 'Not at all' for all food items (fruits, vegetable, meat, dairy, and grain/legumes)",
      Question = "fruits/vegetable/meat/dairy_products/grain_and_legumes",
      Old_value = as.character(paste0(fruits,"/",vegetable,"/",meat,"/",dairy_products,"/",grain_and_legumes)),
      Related_question = "",
      Related_value = ""
    ) |> 
    select(
      all_of(meta_cols),
      Question,
      Old_value,
      Related_question,
      Related_value,
      KEY,
      Issue
    ),
  
  # Inconsistent responses with marital status of respondents
  clean_data$data %>% 
    filter(marital_status %in% c("Divorced","Widowed") & who_in_your_household_worked_2 == 1) %>% 
    mutate(
      Issue = "Inconsistent response with marital status of the respondent!",
      Question = "marital_status",
      Old_value = as.character(marital_status),
      Related_question = "who_in_your_household_worked",
      Related_value = who_in_your_household_worked
    ) |> 
    select(
      all_of(meta_cols),
      Question,
      Old_value,
      Related_question,
      Related_value,
      KEY,
      Issue
    ),
  
  #
  clean_data$data %>% 
    filter(internet == "No" & which_are_some_of_the_sources_of_information_7 == 1) %>% 
    mutate(
      Issue = "The household reported they don't have access to internet but they use internet as one of the sources of information on how to take care of children!",
      Question = "internet",
      Old_value = as.character(internet),
      Related_question = "which_are_some_of_the_sources_of_information",
      Related_value = which_are_some_of_the_sources_of_information
    ) |> 
    select(
      all_of(meta_cols),
      Question,
      Old_value,
      Related_question,
      Related_value,
      KEY,
      Issue
    ),
  
  clean_data$data %>% 
    filter((computer_laptop == "No" & smartphone == "No" & tablet == "No") & which_are_some_of_the_sources_of_information_7 == 1) %>% 
    mutate(
      Issue = "The household reported they don't have access to any devices like laptop/smartphone/tabet but they reported to use them as the sources of information on how to take care of children!",
      Question = "computer_laptop/smartphone/tablet",
      Old_value = as.character(paste0(computer_laptop," / ",smartphone," / ",tablet)),
      Related_question = "which_are_some_of_the_sources_of_information",
      Related_value = which_are_some_of_the_sources_of_information
    ) |> 
    select(
      all_of(meta_cols),
      Question,
      Old_value,
      Related_question,
      Related_value,
      KEY,
      Issue
    ),
  
  clean_data$data %>% 
    filter(radio == "No" & which_are_some_of_the_sources_of_information_5 == 1) %>% 
    mutate(
      Issue = "The household reported they don't have access to Radio but they reported to use it as one of the sources of information on how to take care of children!",
      Question = "radio",
      Old_value = as.character(radio),
      Related_question = "which_are_some_of_the_sources_of_information",
      Related_value = which_are_some_of_the_sources_of_information
    ) |> 
    select(
      all_of(meta_cols),
      Question,
      Old_value,
      Related_question,
      Related_value,
      KEY,
      Issue
    ),
  
  clean_data$data %>% 
    filter(television == "No" & which_are_some_of_the_sources_of_information_6 == 1) %>% 
    mutate(
      Issue = "The household reported they don't have access to television but they reported to use it as one of the sources of information on how to take care of children!",
      Question = "television",
      Old_value = as.character(television),
      Related_question = "which_are_some_of_the_sources_of_information",
      Related_value = which_are_some_of_the_sources_of_information
    ) |> 
    select(
      all_of(meta_cols),
      Question,
      Old_value,
      Related_question,
      Related_value,
      KEY,
      Issue
    ),
  
  # Flagging if option Myself is selected for question 'Who are other HH mems that currently live with you(Respondent):?'
  clean_data$data %>% 
    filter(who_are_other_household_members_0 == 1) %>% 
    mutate(
      Issue = "The respondent should not select him/herself for question 'Who are other HH mems that currently live with you'!",
      Question = "who_are_other_household_members",
      Old_value = as.character(who_are_other_household_members),
      Related_question = "",
      Related_value = ""
    ) |> 
    select(
      all_of(meta_cols),
      Question,
      Old_value,
      Related_question,
      Related_value,
      KEY,
      Issue
    ),
  
  fruits_meat %>% 
    mutate(
      Issue = "The respondent worried about not having enough food during past month, but also reported having 'Nearly everyday' or 'More than half the days' for fruits and meat during past month! (please check other food items as well)",
      Question = "did_you_worry_about_not_having_enough_food",
      Old_value = as.character(did_you_worry_about_not_having_enough_food),
      Related_question = "fruits/meat",
      Related_value = paste0(fruits, "/", meat)
    ) |> 
    select(
      all_of(meta_cols),
      Question,
      Old_value,
      Related_question,
      Related_value,
      KEY,
      Issue
    ),
  
  clean_data$data %>%
    filter(did_you_worry_about_not_having_enough_food == "Yes" & (grain_and_legumes %in% c("Nearly every day", "More than half the days")
                                                                  & dairy_products %in% c("Nearly every day", "More than half the days")
                                                                  & vegetable %in% c("Nearly every day", "More than half the days")
                                                                  & fruits %in% c("Nearly every day", "More than half the days")
                                                                  & meat %in% c("Nearly every day", "More than half the days"))) %>% 
    mutate(
      Issue = "The respondent worried about not having enough food during past month, but also reported having 'Nearly everyday' or 'More than half the days' for all food items during past month!",
      Question = "did_you_worry_about_not_having_enough_food",
      Old_value = as.character(did_you_worry_about_not_having_enough_food),
      Related_question = "fruits/meat",
      Related_value = paste0(fruits, "/", meat)
    ) |> 
    select(
      all_of(meta_cols),
      Question,
      Old_value,
      Related_question,
      Related_value,
      KEY,
      Issue
    ) %>% 
    filter(!KEY %in% fruits_meat$KEY)
  
  
)

lc_child <- rbind(
  # Flagging if any child aged greater than 12 is reported
  clean_data$rpt_bio_children %>% 
    filter(child_years_calc >= 13) %>% 
    mutate(
      Issue = "Child age is greater than 12!",
      Question = "child_years_calc",
      Old_value = as.character(child_years_calc),
      Related_question = "",
      Related_value = ""
    ) |> 
    select(
      all_of(meta_cols),
      Question,
      Old_value,
      Related_question,
      Related_value,
      KEY,
      Issue
    ),
  
  # Flagging if child age is not (potentially) aligned with her/his education level
  clean_data$rpt_bio_children %>% 
    filter(child_years_calc < 6 & current_enrolled_at_any_type_of_education == "Primary school") %>% 
    mutate(
      Issue = paste0("The child age is reported ", child_years_calc, " & ", child_months_calc_mod ," and her/his education level is reporte 'Primary' school.Can you please confirm if it!"),
      Question = "current_enrolled_at_any_type_of_education",
      Old_value = as.character(current_enrolled_at_any_type_of_education),
      Related_question = "child_years_calc",
      Related_value = child_years_calc
    ) |> 
    select(
      all_of(meta_cols),
      Question,
      Old_value,
      Related_question,
      Related_value,
      KEY,
      Issue
    ),
  
  clean_data$rpt_bio_children %>% 
    filter(child_years_calc < 12 & current_enrolled_at_any_type_of_education == "Secondary school") %>% 
    mutate(
      Issue = paste0("The child age is reported ", child_years_calc, " & ", child_months_calc_mod ," and her/his education level is reporte 'Secondary' school.Can you please confirm if it!"),
      Question = "current_enrolled_at_any_type_of_education",
      Old_value = as.character(current_enrolled_at_any_type_of_education),
      Related_question = "child_years_calc",
      Related_value = child_years_calc
    ) |> 
    select(
      all_of(meta_cols),
      Question,
      Old_value,
      Related_question,
      Related_value,
      KEY,
      Issue
    ),
  
  clean_data$rpt_bio_children %>% 
    filter(current_enrolled_at_any_type_of_education %in% c("Higher secondary schools","Technical college / University")) %>% 
    mutate(
      Issue = "Higher Secondary School and College/University is not applicable for child aged between 0-12 yrs old!",
      Question = "current_enrolled_at_any_type_of_education",
      Old_value = as.character(current_enrolled_at_any_type_of_education),
      Related_question = "child_years_calc",
      Related_value = child_years_calc
    ) |> 
    select(
      all_of(meta_cols),
      Question,
      Old_value,
      Related_question,
      Related_value,
      KEY,
      Issue
    ),
  
  # Flagging if both calculated year and month of the child age is reported 0
  clean_data$rpt_bio_children %>% 
    filter(child_years_calc == 0 & child_months_calc_mod == 0) %>% 
    mutate(
      Issue = "The age of the child is incorrect, please rectify the values!",
      Question = "child_years_calc",
      Old_value = as.character(child_years_calc),
      Related_question = "child_months_calc_mod",
      Related_value = child_months_calc_mod
    ) |> 
    select(
      all_of(meta_cols),
      Question,
      Old_value,
      Related_question,
      Related_value,
      KEY,
      Issue
    ),
  
  clean_data$rpt_bio_children %>% 
    filter(confirm_age_child == "No") %>% 
    mutate(
      Issue = "Please find the actual age of the child as it seems the current age is not confirm by respondent!",
      Question = "confirm_age_child",
      Old_value = as.character(confirm_age_child),
      Related_question = "child_age_date_est",
      Related_value = child_age_date_est
    ) |> 
    select(
      all_of(meta_cols),
      Question,
      Old_value,
      Related_question,
      Related_value,
      KEY,
      Issue
    )
)


# table(clean_data$data$phone_number_belongs, useNA = "always")

# Combination of all tools logic checks --------------------------------------
Logic_check_result <- plyr::rbind.fill(
  lc_data,
  lc_child
)

rm(list = ls(pattern = "lc_tool.*"))

