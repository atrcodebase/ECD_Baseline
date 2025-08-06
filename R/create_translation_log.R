source("R/functions/missing_translation_function.R")

# Find missing translations to add in the translation log -----------------
missing_translations <- plyr::rbind.fill(
  ## Tool 0
  plyr::rbind.fill(
    missing_translation_func(clean_data$data) |> mutate(Tab_name = "data"),
    missing_translation_func(clean_data$rpt_bio_children) |> mutate(Tab_name = "rpt_bio_children")
  )
)

need_translation <- c(
  'what_is_your_relationship_with_the_mothers_name_other',
  'Reason_Not_reached_out_to_respondent_Other',
  'Reason_for_not_participating_Other',
  'highest_education_level_completed_other',
  'who_are_other_household_members_other',
  'hh_head_who_other',
  'played_by_whom_other',
  'read_books_or_looked_at_picture_books_by_whom_other',
  'told_stories_or_sang_songs_by_whom_other',
  'named_counted_or_drew_things_by_whom_other',
  'praised_by_whom_other',
  'encourage_social_interaction_by_whom_other',
  'who_usually_decides_how_the_money_you_earn_will_be_used_other',
  'who_usually_makes_decisions_about_health_care_for_yourself_other',
  'who_usually_makes_decisions_about_the_discipline_of_the_kids_other',
  'who_usually_makes_decisions_about_taking_care_of_the_children_in_terms_of_nutrition_other',
  'who_usually_makes_decisions_about_taking_care_of_the_children_in_terms_of_education_other',
  'who_should_earn_money_for_the_family_other',
  'who_should_be_responsible_for_washing_cleaning_and_cooking_other',
  'who_should_be_responsible_for_feeding_and_bathing_children_other',
  'who_should_help_the_children_in_their_studies_at_home_other',
  'who_should_be_responsible_for_looking_after_the_ill_persons_other',
  'who_should_be_responsible_for_disciplining_the_young_kids_other',
  'who_in_your_household_or_between_you_and_your_husband_make_the_decision_other',
  'who_in_your_household_make_the_decision_on_choosing_how_your_children_are_taken_care_other',
  'other_health_problems',
  'own_health_what_type_of_health_services_did_you_get_other',
  'other_health_problems_child',
  'child_health_what_type_of_health_services_did_the_child_get_other',
  'who_in_your_household_worked_other_household_member',
  'which_organizations_did_you_receive_parenting_support_from_other',
  'would_you_like_to_receive_information_on_topics_other',
  'which_are_some_of_the_sources_of_information_other',
  'current_enrolled_at_any_type_of_education_other'
)


missing_translations <- missing_translations %>% 
  filter(question_name %in% need_translation)


# to be removed from environment ------------------------------------------
# remove(missing_translation_func)
# remove(log_translation_cols)
# remove(need_translation)
# rm(missing_translation_final)

