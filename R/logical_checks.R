library(glue)
# LOGIC CHECK
fruits_meat <- clean_data$data %>% 
  filter(did_you_worry_about_not_having_enough_food == "Yes" & (fruits %in% c("Nearly every day", "More than half the days")
                                                                & meat %in% c("Nearly every day","More than half the days"))) 

data_filter <- clean_data$data %>% 
  filter(shouted_yelled_or_screamed_at_child == 0 &
         said_you_would_send_him_her_away == 0 &
         hit_him_her_on_the_bottom_hand_arm_or_leg_with_your_bare_hand == 0 &
         threatened_to_hit_child_but_not_actually_done_it == 0 & 
         hit_him_her_on_the_bottom_hand_arm_or_leg_with_something_like_a_belt_hairbrush_stick_or_some_other_hard_object == 0)

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
    filter(!KEY %in% fruits_meat$KEY),
  
  # Flagging if respodent age is above 40 which makes her not illegible
  clean_data$data %>% 
    filter(age_in_last_birthday > 40) %>% 
    mutate(
      Issue = "The respondent age is reporter greater than 40 yrs old!",
      Question = "age_in_last_birthday",
      Old_value = as.character(age_in_last_birthday),
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
  
  clean_data$data %>% 
    mutate(
      total_hh_mem_by_type = rowSums(select(.,starts_with("who_are_other_household_members_") & !matches("who_are_other_household_members_Other")), na.rm = T)
    ) %>%  
    filter(total_hh_mem_by_type >= how_many_people_currently_live_in_your_household) %>% 
    mutate(
      Issue = "In question 'who_are_other_household_members' (She should not count herself), the respondent reported more/equal number of HH members as the HH size (She should count herself)!",
      Question = "how_many_people_currently_live_in_your_household",
      Old_value = as.character(how_many_people_currently_live_in_your_household),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
  
  # Flagging if the Household head is not listed in the 'who_are_other_household_members'
  clean_data$data %>% 
    filter(hh_head_who == "Husband" & who_are_other_household_members_1 == 0) %>% 
    mutate(
      Issue = "Husband is reported as the Household head, but also Husband is not reported as a member of household!",
      Question = "hh_head_who",
      Old_value = as.character(hh_head_who),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(hh_head_who == "Father" & who_are_other_household_members_2 == 0) %>% 
    mutate(
      Issue = "Father is reported as the Household head, but also Father is not reported as a member of household!",
      Question = "hh_head_who",
      Old_value = as.character(hh_head_who),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(hh_head_who == "Mother" & who_are_other_household_members_3 == 0) %>% 
    mutate(
      Issue = "Mother is reported as the Household head, but also Mother is not reported as a member of household!",
      Question = "hh_head_who",
      Old_value = as.character(hh_head_who),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(hh_head_who == "Father-in-law (father of husband)" & who_are_other_household_members_4 == 0) %>% 
    mutate(
      Issue = "Father-in-law (father of husband) is reported as the Household head, but also Father-in-law (father of husband) is not reported as a member of household!",
      Question = "hh_head_who",
      Old_value = as.character(hh_head_who),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(hh_head_who == "Mother-in-law (mother of husband)" & who_are_other_household_members_5 == 0) %>% 
    mutate(
      Issue = "Mother-in-law (mother of husband) is reported as the Household head, but also Mother-in-law (mother of husband) is not reported as a member of household!",
      Question = "hh_head_who",
      Old_value = as.character(hh_head_who),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(hh_head_who == "Brothers" & who_are_other_household_members_6 == 0) %>% 
    mutate(
      Issue = "Brothers is reported as the Household head, but also Brothers is not reported as a member of household!",
      Question = "hh_head_who",
      Old_value = as.character(hh_head_who),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(hh_head_who == "Sisters" & who_are_other_household_members_7 == 0) %>% 
    mutate(
      Issue = "Sisters is reported as the Household head, but also Sisters is not reported as a member of household!",
      Question = "hh_head_who",
      Old_value = as.character(hh_head_who),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(hh_head_who == "Children" & who_are_other_household_members_8 == 0) %>% 
    mutate(
      Issue = "Children is reported as the Household head, but also Children is not reported as a member of household!",
      Question = "hh_head_who",
      Old_value = as.character(hh_head_who),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(hh_head_who == "Brother-in-law (brother of husband)" & who_are_other_household_members_10 == 0) %>% 
    mutate(
      Issue = "Brother-in-law (brother of husband) is reported as the Household head, but also Brother-in-law (brother of husband) is not reported as a member of household!",
      Question = "hh_head_who",
      Old_value = as.character(hh_head_who),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(hh_head_who == "Sister-in-law (sister of husband)" & who_are_other_household_members_11 == 0) %>% 
    mutate(
      Issue = "Sister-in-law (sister of husband) is reported as the Household head, but also Sister-in-law (sister of husband) is not reported as a member of household!",
      Question = "hh_head_who",
      Old_value = as.character(hh_head_who),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(hh_head_who == "Nephews" & who_are_other_household_members_12 == 0) %>% 
    mutate(
      Issue = "Nephews is reported as the Household head, but also Nephews is not reported as a member of household!",
      Question = "hh_head_who",
      Old_value = as.character(hh_head_who),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(hh_head_who == "Niece" & who_are_other_household_members_13 == 0) %>% 
    mutate(
      Issue = "Niece is reported as the Household head, but also Niece is not reported as a member of household!",
      Question = "hh_head_who",
      Old_value = as.character(hh_head_who),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(hh_head_who == "Grandfather" & who_are_other_household_members_14 == 0) %>% 
    mutate(
      Issue = "Grandfather is reported as the Household head, but also Grandfather is not reported as a member of household!",
      Question = "hh_head_who",
      Old_value = as.character(hh_head_who),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(hh_head_who == "Grandmother" & who_are_other_household_members_15 == 0) %>% 
    mutate(
      Issue = "Grandmother is reported as the Household head, but also Grandmother is not reported as a member of household!",
      Question = "hh_head_who",
      Old_value = as.character(hh_head_who),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
  
  # Flagging if age-based questions are skipped or needs values
  # 2yrs
  clean_data$data %>% 
    filter((eldest_child_age.re_calc >= 24 & eldest_child_age.re_calc < 36) & is.na(communication_chid_aged_2)) %>% 
    mutate(
      Issue = "The eldest child's age is between 24 and 36 months (2 yrs) but the age-based questions for 2 yrs old are blank!",
      Question = "eldest_child_age.re_calc | eldest_child_age",
      Old_value = as.character(paste0(eldest_child_age.re_calc, " | ", eldest_child_age)),
      Related_question = "communication_chid_aged_2(As an instance)",
      Related_value = communication_chid_aged_2
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
    filter((eldest_child_age.re_calc < 24 | eldest_child_age.re_calc >= 36) & !is.na(communication_chid_aged_2)) %>% 
    mutate(
      Issue = "The eldest child's age is not between 24 and 36 months (2 yrs) but the age-based questions for 2 yrs old have values!",
      Question = "eldest_child_age.re_calc | eldest_child_age",
      Old_value = as.character(paste0(eldest_child_age.re_calc, " | ", eldest_child_age)),
      Related_question = "communication_chid_aged_2(As an instance)",
      Related_value = communication_chid_aged_2
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
  # 3yrs
  clean_data$data %>% 
    filter((eldest_child_age.re_calc >= 36 & eldest_child_age.re_calc < 48) & is.na(communication_chid_aged_3)) %>% 
    mutate(
      Issue = "The eldest child's age is between 36 and 48 months (3 yrs) but the age-based questions for 3 yrs old are blank!",
      Question = "eldest_child_age.re_calc | eldest_child_age",
      Old_value = as.character(paste0(eldest_child_age.re_calc, " | ", eldest_child_age)),
      Related_question = "communication_chid_aged_3(As an instance)",
      Related_value = communication_chid_aged_3
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
    filter((eldest_child_age.re_calc < 36 | eldest_child_age.re_calc >= 48) & !is.na(communication_chid_aged_3)) %>% 
    mutate(
      Issue = "The eldest child's age is not between 36 and 48 months (3 yrs) but the age-based questions for 3 yrs old have values!",
      Question = "eldest_child_age.re_calc | eldest_child_age",
      Old_value = as.character(paste0(eldest_child_age.re_calc, " | ", eldest_child_age)),
      Related_question = "communication_chid_aged_3(As an instance)",
      Related_value = communication_chid_aged_3
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
  # 4yrs
  clean_data$data %>% 
    filter((eldest_child_age.re_calc >= 48 & eldest_child_age.re_calc < 60) & is.na(communication_1_chid_aged_4)) %>% 
    mutate(
      Issue = "The eldest child's age is between 48 and 60 months (4 yrs) but the age-based questions for 4 yrs old are blank!",
      Question = "eldest_child_age.re_calc | eldest_child_age",
      Old_value = as.character(paste0(eldest_child_age.re_calc, " | ", eldest_child_age)),
      Related_question = "communication_1_chid_aged_4(As an instance)",
      Related_value = communication_1_chid_aged_4
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
    filter((eldest_child_age.re_calc < 48 | eldest_child_age.re_calc >= 60) & !is.na(communication_1_chid_aged_4)) %>% 
    mutate(
      Issue = "The eldest child's age is not between 48 and 60 months (4 yrs) but the age-based questions for 4 yrs old have values!",
      Question = "eldest_child_age.re_calc | eldest_child_age",
      Old_value = as.character(paste0(eldest_child_age.re_calc, " | ", eldest_child_age)),
      Related_question = "communication_1_chid_aged_4(As an instance)",
      Related_value = communication_1_chid_aged_4
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
  # 5yrs
  clean_data$data %>% 
    filter((eldest_child_age.re_calc >= 60 & eldest_child_age.re_calc < 84) & is.na(communication_chid_aged_5_6)) %>% 
    mutate(
      Issue = "The eldest child's age is between 60 and 84 months (5-6 yrs) but the age-based questions for 5-6 yrs old are blank!",
      Question = "eldest_child_age.re_calc | eldest_child_age",
      Old_value = as.character(paste0(eldest_child_age.re_calc, " | ", eldest_child_age)),
      Related_question = "communication_chid_aged_5_6(As an instance)",
      Related_value = communication_chid_aged_5_6
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
    filter((eldest_child_age.re_calc < 60 | eldest_child_age.re_calc >= 84) & !is.na(communication_chid_aged_5_6)) %>% 
    mutate(
      Issue = "The eldest child's age is not between 60 and 84 months (5-6 yrs) but the age-based questions for 5-6 yrs old have values!",
      Question = "eldest_child_age.re_calc | eldest_child_age",
      Old_value = as.character(paste0(eldest_child_age.re_calc, " | ", eldest_child_age)),
      Related_question = "communication_chid_aged_5_6(As an instance)",
      Related_value = communication_chid_aged_5_6
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
  
  # Flagging if marital status is reported either Widowed or Divorced but also reporter child played with father
  clean_data$data %>% 
    filter(marital_status == "Widowed" & (played_by_whom == "Father" |
                                            read_books_or_looked_at_picture_books_by_whom == "Father" |
                                            told_stories_or_sang_songs_by_whom == "Father" |
                                            named_counted_or_drew_things_by_whom == "Father" |
                                            praised_by_whom == "Father" |
                                            encourage_social_interaction_by_whom == "Father")) %>% 
    mutate(
      Issue = "The martial status of respondent is reported ' Widowed', while she reported 'Father' was involved in one of those 6 activities with child during past 7 days!",
      Question = "marital_status",
      Old_value = as.character(marital_status),
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
  
  clean_data$data %>% 
    filter(marital_status == "Divorced" & (played_by_whom == "Father" |
                                            read_books_or_looked_at_picture_books_by_whom == "Father" |
                                            told_stories_or_sang_songs_by_whom == "Father" |
                                            named_counted_or_drew_things_by_whom == "Father" |
                                            praised_by_whom == "Father" |
                                            encourage_social_interaction_by_whom == "Father")) %>% 
    mutate(
      Issue = "The martial status of respondent is reported 'Divorced', while she reported 'Father' was involved in one of those 6 activities with child during past 7 days!(Please confirm the response)",
      Question = "marital_status",
      Old_value = as.character(marital_status),
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
  
  # Flagging if any activities reported 0 days and 0 minutes
  clean_data$data %>% 
    filter(played_days == 0) %>% 
    mutate(
      Issue = "0 days is not allowed to be selected! if so, 'No Activity done' should be selected for question played_by_whom!",
      Question = "played_days",
      Old_value = as.character(played_days),
      Related_question = "played_by_whom",
      Related_value = played_by_whom
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
    filter(played_minutes == 0) %>% 
    mutate(
      Issue = "0 minutes is not allowed to be selected! if so, 'No Activity done' should be selected for question played_by_whom!",
      Question = "played_minutes",
      Old_value = as.character(played_minutes),
      Related_question = "played_by_whom",
      Related_value = played_by_whom
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
    filter(read_books_or_looked_at_picture_books_days == 0) %>% 
    mutate(
      Issue = "0 days is not allowed to be selected! if so, 'No Activity done' should be selected for question read_books_or_looked_at_picture_books_by_whom!",
      Question = "read_books_or_looked_at_picture_books_days",
      Old_value = as.character(read_books_or_looked_at_picture_books_days),
      Related_question = "read_books_or_looked_at_picture_books_by_whom",
      Related_value = read_books_or_looked_at_picture_books_by_whom
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
    filter(read_books_or_looked_at_picture_books_minutes == 0) %>% 
    mutate(
      Issue = "0 minutes is not allowed to be selected! if so, 'No Activity done' should be selected for question read_books_or_looked_at_picture_books_by_whom!",
      Question = "read_books_or_looked_at_picture_books_minutes",
      Old_value = as.character(read_books_or_looked_at_picture_books_minutes),
      Related_question = "read_books_or_looked_at_picture_books_by_whom",
      Related_value = read_books_or_looked_at_picture_books_by_whom
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
    filter(told_stories_or_sang_songs_days == 0) %>% 
    mutate(
      Issue = "0 days is not allowed to be selected! if so, 'No Activity done' should be selected for question told_stories_or_sang_songs_by_whom!",
      Question = "told_stories_or_sang_songs_days",
      Old_value = as.character(told_stories_or_sang_songs_days),
      Related_question = "told_stories_or_sang_songs_by_whom",
      Related_value = told_stories_or_sang_songs_by_whom
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
    filter(told_stories_or_sang_songs_minutes == 0) %>% 
    mutate(
      Issue = "0 minutes is not allowed to be selected! if so, 'No Activity done' should be selected for question told_stories_or_sang_songs_by_whom!",
      Question = "told_stories_or_sang_songs_minutes",
      Old_value = as.character(told_stories_or_sang_songs_minutes),
      Related_question = "told_stories_or_sang_songs_by_whom",
      Related_value = told_stories_or_sang_songs_by_whom
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
    filter(named_counted_or_drew_things_days == 0) %>% 
    mutate(
      Issue = "0 days is not allowed to be selected! if so, 'No Activity done' should be selected for question named_counted_or_drew_things_by_whom!",
      Question = "named_counted_or_drew_things_days",
      Old_value = as.character(named_counted_or_drew_things_days),
      Related_question = "named_counted_or_drew_things_by_whom",
      Related_value = named_counted_or_drew_things_by_whom
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
    filter(named_counted_or_drew_things_minutes == 0) %>% 
    mutate(
      Issue = "0 minutes is not allowed to be selected! if so, 'No Activity done' should be selected for question named_counted_or_drew_things_by_whom!",
      Question = "named_counted_or_drew_things_minutes",
      Old_value = as.character(named_counted_or_drew_things_minutes),
      Related_question = "named_counted_or_drew_things_by_whom",
      Related_value = named_counted_or_drew_things_by_whom
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
    filter(praised_days == 0) %>% 
    mutate(
      Issue = "0 days is not allowed to be selected! if so, 'No Activity done' should be selected for question praised_by_whom!",
      Question = "praised_days",
      Old_value = as.character(praised_days),
      Related_question = "praised_by_whom",
      Related_value = praised_by_whom
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
    filter(praised_times == 0) %>% 
    mutate(
      Issue = "0 times is not allowed to be selected! if so, 'No Activity done' should be selected for question praised_by_whom!",
      Question = "praised_times",
      Old_value = as.character(praised_times),
      Related_question = "praised_by_whom",
      Related_value = praised_by_whom
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
    filter(encourage_social_interaction_days == 0) %>% 
    mutate(
      Issue = "0 days is not allowed to be selected! if so, 'No Activity done' should be selected for question encourage_social_interaction_by_whom!",
      Question = "encourage_social_interaction_days",
      Old_value = as.character(encourage_social_interaction_days),
      Related_question = "encourage_social_interaction_by_whom",
      Related_value = encourage_social_interaction_by_whom
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
    filter(encourage_social_interaction_times == 0) %>% 
    mutate(
      Issue = "0 times is not allowed to be selected! if so, 'No Activity done' should be selected for question encourage_social_interaction_by_whom!",
      Question = "encourage_social_interaction_times",
      Old_value = as.character(encourage_social_interaction_times),
      Related_question = "encourage_social_interaction_by_whom",
      Related_value = encourage_social_interaction_by_whom
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
  
  # who_should_earn_money_for_the_family
  clean_data$data %>% 
    filter(who_should_earn_money_for_the_family == "Myself and my father" & who_are_other_household_members_2 == 0) %>% 
    mutate(
      Issue = "'Father' is reported for question who should earn money for the family but 'Father' is not reported as a member of family living with her!",
      Question = "who_should_earn_money_for_the_family",
      Old_value = as.character(who_should_earn_money_for_the_family),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_should_earn_money_for_the_family == "Myself and my mother" & who_are_other_household_members_3 == 0) %>% 
    mutate(
      Issue = "'Mother' is reported for question who should earn money for the family but 'Mother' is not reported as a member of family living with her!",
      Question = "who_should_earn_money_for_the_family",
      Old_value = as.character(who_should_earn_money_for_the_family),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_should_earn_money_for_the_family == "Myself and my father-in-law" & who_are_other_household_members_4 == 0) %>% 
    mutate(
      Issue = "'Father-in-law' is reported for question who should earn money for the family but 'Father-in-law' is not reported as a member of family living with her!",
      Question = "who_should_earn_money_for_the_family",
      Old_value = as.character(who_should_earn_money_for_the_family),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_should_earn_money_for_the_family == "Myself and my mother-in-law" & who_are_other_household_members_5 == 0) %>% 
    mutate(
      Issue = "'Mother-in-law' is reported for question who should earn money for the family but 'Mother-in-law' is not reported as a member of family living with her!",
      Question = "who_should_earn_money_for_the_family",
      Old_value = as.character(who_should_earn_money_for_the_family),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
  
  # who_should_be_responsible_for_washing_cleaning_and_cooking
  clean_data$data %>% 
    filter(who_should_be_responsible_for_washing_cleaning_and_cooking == "Myself and my father" & who_are_other_household_members_2 == 0) %>% 
    mutate(
      Issue = "'Father' is reported for question who should earn money for the family but 'Father' is not reported as a member of family living with her!",
      Question = "who_should_be_responsible_for_washing_cleaning_and_cooking",
      Old_value = as.character(who_should_be_responsible_for_washing_cleaning_and_cooking),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_should_be_responsible_for_washing_cleaning_and_cooking == "Myself and my mother" & who_are_other_household_members_3 == 0) %>% 
    mutate(
      Issue = "'Mother' is reported for question who should earn money for the family but 'Mother' is not reported as a member of family living with her!",
      Question = "who_should_be_responsible_for_washing_cleaning_and_cooking",
      Old_value = as.character(who_should_be_responsible_for_washing_cleaning_and_cooking),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_should_be_responsible_for_washing_cleaning_and_cooking == "Myself and my father-in-law" & who_are_other_household_members_4 == 0) %>% 
    mutate(
      Issue = "'Father-in-law' is reported for question who should earn money for the family but 'Father-in-law' is not reported as a member of family living with her!",
      Question = "who_should_be_responsible_for_washing_cleaning_and_cooking",
      Old_value = as.character(who_should_be_responsible_for_washing_cleaning_and_cooking),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_should_be_responsible_for_washing_cleaning_and_cooking == "Myself and my mother-in-law" & who_are_other_household_members_5 == 0) %>% 
    mutate(
      Issue = "'Mother-in-law' is reported for question who should earn money for the family but 'Mother-in-law' is not reported as a member of family living with her!",
      Question = "who_should_be_responsible_for_washing_cleaning_and_cooking",
      Old_value = as.character(who_should_be_responsible_for_washing_cleaning_and_cooking),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
  
  # who_should_be_responsible_for_feeding_and_bathing_children
  clean_data$data %>% 
    filter(who_should_be_responsible_for_feeding_and_bathing_children == "Myself and my father" & who_are_other_household_members_2 == 0) %>% 
    mutate(
      Issue = "'Father' is reported for question who should earn money for the family but 'Father' is not reported as a member of family living with her!",
      Question = "who_should_be_responsible_for_feeding_and_bathing_children",
      Old_value = as.character(who_should_be_responsible_for_feeding_and_bathing_children),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_should_be_responsible_for_feeding_and_bathing_children == "Myself and my mother" & who_are_other_household_members_3 == 0) %>% 
    mutate(
      Issue = "'Mother' is reported for question who should earn money for the family but 'Mother' is not reported as a member of family living with her!",
      Question = "who_should_be_responsible_for_feeding_and_bathing_children",
      Old_value = as.character(who_should_be_responsible_for_feeding_and_bathing_children),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_should_be_responsible_for_feeding_and_bathing_children == "Myself and my father-in-law" & who_are_other_household_members_4 == 0) %>% 
    mutate(
      Issue = "'Father-in-law' is reported for question who should earn money for the family but 'Father-in-law' is not reported as a member of family living with her!",
      Question = "who_should_be_responsible_for_feeding_and_bathing_children",
      Old_value = as.character(who_should_be_responsible_for_feeding_and_bathing_children),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_should_be_responsible_for_feeding_and_bathing_children == "Myself and my mother-in-law" & who_are_other_household_members_5 == 0) %>% 
    mutate(
      Issue = "'Mother-in-law' is reported for question who should earn money for the family but 'Mother-in-law' is not reported as a member of family living with her!",
      Question = "who_should_be_responsible_for_feeding_and_bathing_children",
      Old_value = as.character(who_should_be_responsible_for_feeding_and_bathing_children),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
  
  # who_should_help_the_children_in_their_studies_at_home
  clean_data$data %>% 
    filter(who_should_help_the_children_in_their_studies_at_home == "Myself and my father" & who_are_other_household_members_2 == 0) %>% 
    mutate(
      Issue = "'Father' is reported for question who should earn money for the family but 'Father' is not reported as a member of family living with her!",
      Question = "who_should_help_the_children_in_their_studies_at_home",
      Old_value = as.character(who_should_help_the_children_in_their_studies_at_home),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_should_help_the_children_in_their_studies_at_home == "Myself and my mother" & who_are_other_household_members_3 == 0) %>% 
    mutate(
      Issue = "'Mother' is reported for question who should earn money for the family but 'Mother' is not reported as a member of family living with her!",
      Question = "who_should_help_the_children_in_their_studies_at_home",
      Old_value = as.character(who_should_help_the_children_in_their_studies_at_home),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_should_help_the_children_in_their_studies_at_home == "Myself and my father-in-law" & who_are_other_household_members_4 == 0) %>% 
    mutate(
      Issue = "'Father-in-law' is reported for question who should earn money for the family but 'Father-in-law' is not reported as a member of family living with her!",
      Question = "who_should_help_the_children_in_their_studies_at_home",
      Old_value = as.character(who_should_help_the_children_in_their_studies_at_home),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_should_help_the_children_in_their_studies_at_home == "Myself and my mother-in-law" & who_are_other_household_members_5 == 0) %>% 
    mutate(
      Issue = "'Mother-in-law' is reported for question who should earn money for the family but 'Mother-in-law' is not reported as a member of family living with her!",
      Question = "who_should_help_the_children_in_their_studies_at_home",
      Old_value = as.character(who_should_help_the_children_in_their_studies_at_home),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
  
  # who_should_be_responsible_for_looking_after_the_ill_persons
  clean_data$data %>% 
    filter(who_should_be_responsible_for_looking_after_the_ill_persons == "Myself and my father" & who_are_other_household_members_2 == 0) %>% 
    mutate(
      Issue = "'Father' is reported for question who should earn money for the family but 'Father' is not reported as a member of family living with her!",
      Question = "who_should_be_responsible_for_looking_after_the_ill_persons",
      Old_value = as.character(who_should_be_responsible_for_looking_after_the_ill_persons),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_should_be_responsible_for_looking_after_the_ill_persons == "Myself and my mother" & who_are_other_household_members_3 == 0) %>% 
    mutate(
      Issue = "'Mother' is reported for question who should earn money for the family but 'Mother' is not reported as a member of family living with her!",
      Question = "who_should_be_responsible_for_looking_after_the_ill_persons",
      Old_value = as.character(who_should_be_responsible_for_looking_after_the_ill_persons),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_should_be_responsible_for_looking_after_the_ill_persons == "Myself and my father-in-law" & who_are_other_household_members_4 == 0) %>% 
    mutate(
      Issue = "'Father-in-law' is reported for question who should earn money for the family but 'Father-in-law' is not reported as a member of family living with her!",
      Question = "who_should_be_responsible_for_looking_after_the_ill_persons",
      Old_value = as.character(who_should_be_responsible_for_looking_after_the_ill_persons),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_should_be_responsible_for_looking_after_the_ill_persons == "Myself and my mother-in-law" & who_are_other_household_members_5 == 0) %>% 
    mutate(
      Issue = "'Mother-in-law' is reported for question who should earn money for the family but 'Mother-in-law' is not reported as a member of family living with her!",
      Question = "who_should_be_responsible_for_looking_after_the_ill_persons",
      Old_value = as.character(who_should_be_responsible_for_looking_after_the_ill_persons),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
  
  # who_should_be_responsible_for_disciplining_the_young_kids
  clean_data$data %>% 
    filter(who_should_be_responsible_for_disciplining_the_young_kids == "Myself and my father" & who_are_other_household_members_2 == 0) %>% 
    mutate(
      Issue = "'Father' is reported for question who should earn money for the family but 'Father' is not reported as a member of family living with her!",
      Question = "who_should_be_responsible_for_disciplining_the_young_kids",
      Old_value = as.character(who_should_be_responsible_for_disciplining_the_young_kids),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_should_be_responsible_for_disciplining_the_young_kids == "Myself and my mother" & who_are_other_household_members_3 == 0) %>% 
    mutate(
      Issue = "'Mother' is reported for question who should earn money for the family but 'Mother' is not reported as a member of family living with her!",
      Question = "who_should_be_responsible_for_disciplining_the_young_kids",
      Old_value = as.character(who_should_be_responsible_for_disciplining_the_young_kids),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_should_be_responsible_for_disciplining_the_young_kids == "Myself and my father-in-law" & who_are_other_household_members_4 == 0) %>% 
    mutate(
      Issue = "'Father-in-law' is reported for question who should earn money for the family but 'Father-in-law' is not reported as a member of family living with her!",
      Question = "who_should_be_responsible_for_disciplining_the_young_kids",
      Old_value = as.character(who_should_be_responsible_for_disciplining_the_young_kids),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_should_be_responsible_for_disciplining_the_young_kids == "Myself and my mother-in-law" & who_are_other_household_members_5 == 0) %>% 
    mutate(
      Issue = "'Mother-in-law' is reported for question who should earn money for the family but 'Mother-in-law' is not reported as a member of family living with her!",
      Question = "who_should_be_responsible_for_disciplining_the_young_kids",
      Old_value = as.character(who_should_be_responsible_for_disciplining_the_young_kids),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
  
  # who_in_your_household_or_between_you_and_your_husband_make_the_decision
  clean_data$data %>% 
    filter(who_in_your_household_or_between_you_and_your_husband_make_the_decision == "Myself and my father" & who_are_other_household_members_2 == 0) %>% 
    mutate(
      Issue = "'Father' is reported for question who should earn money for the family but 'Father' is not reported as a member of family living with her!",
      Question = "who_in_your_household_or_between_you_and_your_husband_make_the_decision",
      Old_value = as.character(who_in_your_household_or_between_you_and_your_husband_make_the_decision),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_in_your_household_or_between_you_and_your_husband_make_the_decision == "Myself and my mother" & who_are_other_household_members_3 == 0) %>% 
    mutate(
      Issue = "'Mother' is reported for question who should earn money for the family but 'Mother' is not reported as a member of family living with her!",
      Question = "who_in_your_household_or_between_you_and_your_husband_make_the_decision",
      Old_value = as.character(who_in_your_household_or_between_you_and_your_husband_make_the_decision),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_in_your_household_or_between_you_and_your_husband_make_the_decision == "Myself and my father-in-law" & who_are_other_household_members_4 == 0) %>% 
    mutate(
      Issue = "'Father-in-law' is reported for question who should earn money for the family but 'Father-in-law' is not reported as a member of family living with her!",
      Question = "who_in_your_household_or_between_you_and_your_husband_make_the_decision",
      Old_value = as.character(who_in_your_household_or_between_you_and_your_husband_make_the_decision),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_in_your_household_or_between_you_and_your_husband_make_the_decision == "Myself and my mother-in-law" & who_are_other_household_members_5 == 0) %>% 
    mutate(
      Issue = "'Mother-in-law' is reported for question who should earn money for the family but 'Mother-in-law' is not reported as a member of family living with her!",
      Question = "who_in_your_household_or_between_you_and_your_husband_make_the_decision",
      Old_value = as.character(who_in_your_household_or_between_you_and_your_husband_make_the_decision),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
  
  # who_in_your_household_make_the_decision_on_choosing_how_your_children_are_taken_care
  clean_data$data %>% 
    filter(who_in_your_household_make_the_decision_on_choosing_how_your_children_are_taken_care == "Myself and my father" & who_are_other_household_members_2 == 0) %>% 
    mutate(
      Issue = "'Father' is reported for question who should earn money for the family but 'Father' is not reported as a member of family living with her!",
      Question = "who_in_your_household_make_the_decision_on_choosing_how_your_children_are_taken_care",
      Old_value = as.character(who_in_your_household_make_the_decision_on_choosing_how_your_children_are_taken_care),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_in_your_household_make_the_decision_on_choosing_how_your_children_are_taken_care == "Myself and my mother" & who_are_other_household_members_3 == 0) %>% 
    mutate(
      Issue = "'Mother' is reported for question who should earn money for the family but 'Mother' is not reported as a member of family living with her!",
      Question = "who_in_your_household_make_the_decision_on_choosing_how_your_children_are_taken_care",
      Old_value = as.character(who_in_your_household_make_the_decision_on_choosing_how_your_children_are_taken_care),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_in_your_household_make_the_decision_on_choosing_how_your_children_are_taken_care == "Myself and my father-in-law" & who_are_other_household_members_4 == 0) %>% 
    mutate(
      Issue = "'Father-in-law' is reported for question who should earn money for the family but 'Father-in-law' is not reported as a member of family living with her!",
      Question = "who_in_your_household_make_the_decision_on_choosing_how_your_children_are_taken_care",
      Old_value = as.character(who_in_your_household_make_the_decision_on_choosing_how_your_children_are_taken_care),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(who_in_your_household_make_the_decision_on_choosing_how_your_children_are_taken_care == "Myself and my mother-in-law" & who_are_other_household_members_5 == 0) %>% 
    mutate(
      Issue = "'Mother-in-law' is reported for question who should earn money for the family but 'Mother-in-law' is not reported as a member of family living with her!",
      Question = "who_in_your_household_make_the_decision_on_choosing_how_your_children_are_taken_care",
      Old_value = as.character(who_in_your_household_make_the_decision_on_choosing_how_your_children_are_taken_care),
      Related_question = "who_are_other_household_members",
      Related_value = who_are_other_household_members
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
    filter(hh_head_who == "Husband" & (who_in_your_household_worked_1 == 1 & who_in_your_household_worked_2 == 1)) %>% 
    mutate(
      Issue = "Husband is reported as the head of household in question hh_head_who, therefore both 'Head of household' and 'Husband' can be selected at the same time for question who_in_your_household_worked!",
      Question = "hh_head_who",
      Old_value = as.character(hh_head_who),
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
  clean_data$data %>% 
    filter(hh_head_who == "Myself (Respondent)" & (who_in_your_household_worked_1 == 1 & who_in_your_household_worked_3 == 1)) %>% 
    mutate(
      Issue = "'Myself' is reported as the head of household in question hh_head_who, therefore both 'Head of household' and 'Myself' can be selected at the same time for question who_in_your_household_worked!",
      Question = "hh_head_who",
      Old_value = as.character(hh_head_who),
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
  
  clean_data$data %>% 
    filter(how_much_do_you_estimate_is_your_households_income_today == 0 & how_is_your_income_today_compared_to_3_years_ago == "Increased") %>% 
    mutate(
      Issue = "The amount of income reported 0 but the the amount compared to last 3 years ago is reported 'Increased'!",
      Question = "how_much_do_you_estimate_is_your_households_income_today",
      Old_value = as.character(how_much_do_you_estimate_is_your_households_income_today),
      Related_question = "how_is_your_income_today_compared_to_3_years_ago",
      Related_value = how_is_your_income_today_compared_to_3_years_ago
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
    filter(child_years_calc < 5 & current_enrolled_at_any_type_of_education == "Primary school") %>% 
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
    filter(child_years_calc < 11 & current_enrolled_at_any_type_of_education == "Secondary school") %>% 
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
    ),
  
  clean_data$rpt_bio_children %>% 
    filter(child_age_date_est == "Yes, enter exact birth date" & is.na(child_date)) %>% 
    mutate(
      Issue = "Please enter the exact birthdate of the child, the cell can not be blank!",
      Question = "child_age_date_est",
      Old_value = as.character(child_age_date_est),
      Related_question = "child_date",
      Related_value = child_date
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
    filter(child_age_date_est == "Yes, enter exact birth date" & !is.na(child_age_est_years)) %>% 
    mutate(
      Issue = "Please remove the value from question child_age_est_years!",
      Question = "child_age_date_est",
      Old_value = as.character(child_age_date_est),
      Related_question = "child_age_est_years",
      Related_value = child_age_est_years
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
    filter(child_age_date_est == "Yes, enter exact birth date" & !is.na(child_age_est_months)) %>% 
    mutate(
      Issue = "Please remove the value from question child_age_est_months!",
      Question = "child_age_date_est",
      Old_value = as.character(child_age_date_est),
      Related_question = "child_age_est_months",
      Related_value = child_age_est_months
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
    filter(child_age_date_est == "No, estimated age" & !is.na(child_date)) %>% 
    mutate(
      Issue = "Please remove the date from question child_date!",
      Question = "child_age_date_est",
      Old_value = as.character(child_age_date_est),
      Related_question = "child_date",
      Related_value = child_date
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

