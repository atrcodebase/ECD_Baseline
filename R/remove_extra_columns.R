# getting the column names to exclude from data set for client ------------
qa_cols <- c("review_status", "review_quality", "review_comments", "review_corrections")
# DONE
extra_cols_data <- c(
  # Meta Cols
  "deviceid",'Subscriberid','Simid','Devicephonenum','Username','TA','AA','duration','caseid','instance_time','pct_conversation','Passcode',
  'Surveyor_Name','Surveyor_Gender_Paused','phone_call_log','phone_call_duration','collect_phone_app','device_info','resp_pn','full_name',
  'full_name_PD','address','users','pub_to_users','call_datetime','callback_time','new_sortby','last_call_status','num_calls','call_num',
  'stop_at','needs_review','now_complete','calltime', 'TPMA_Location_Name', 'TPMA_Location_ID','Village','CDC_CCDC_Gozar_Name',
  'CDC_CCDC_Gozar_ID','Region', 
  
  #PII 
  'parent_name','parent_father_name','resp_tazkira_no','sample_randomization','ECD_master_trainer','ECD_facilitator','Center',
  'info_respondent','call_respondent','updated_ph_number','AA2','please_provide_the_phone_number','time_for_callback_08_50_22_320_04_30',
  'time_for_callback_09_33_59_288_04_30','time_for_callback_09_58_25_725_04_30','time_for_callback_10_32_58_999Z','time_for_callback_10_53_15_215_04_30',
  'time_for_callback_10_42_02_381_04_30', 
  'what_is_your_full_name', 'what_is_your_father_name','what_is_the_family_household_name','another_hh_full_name','another_hh_father_name',
  'AA3', 'rpt_bio_children_count','SET-OF-rpt_bio_children',
  'n_children06',	'eldest_child_age',	'eldest_child_age_yrs', # To be confirmed
  'eldest_child_name','child2_years','child3_years','child4_years','child5_years','child6_years','AA4','child_age_2_note',
  'child_age_3_note', 'child_age_4_note', 'child_age_5_6_note', 'AA5', 'AA6','strengths_and_difficulties_note', 'AA7','AA8','AA9',
  'would_you_like_to_receive_information_on_topics_5', 'surveyor_comment1',	'surveyor_comment1_audio', 'surveyor_comment1_audio_translation',
  'surveyor_comment2',	'surveyor_comment2_audio', 'surveyor_comment2_audio_translation',	'Survey_Language',	'Survey_Language_Other',
  'interview_status', 'reschedule',	'reschedule_full',	'reschedule_no_ans',	'now_c_dari',	'now_c_pashto',	'instanceID',	'formdef_version',
  'qa_log_status',	'eldest_child_age.re_calc',	'eldest_child_age_yrs.re_calc',
  
  'Can_We_Callback1',	'Can_We_Callback2',	'Reason_for_not_participating',	'Reason_for_not_participating_Other',	'Can_We_Callback',
  'right_time_callback',	'right_time_callback_1',	'right_time_callback_2',	'right_time_callback_3',	'right_time_callback_4',	'right_time_callback_5',
  'right_time_callback_6',	'right_time_callback_7',	'time_for_callback',	'time_for_callback_1',	'time_for_callback_2',	'time_for_callback_3',
  'time_for_callback_4', "who_are_other_household_members_9", "do_you_have_access_to"
  
)

extra_cols_child <- c(
  "child_name", 'SET-OF-rpt_bio_children', 'qa_log_status',	'SubmissionDate',	'Starttime',	'Endtime',
  'child_years_calc','child_months_calc','child_months_calc_mod',
  'child_years_calc.re_calc',	'child_age_est_years_mult_12',	'child_months_calc.re_calc',	'child_months_calc_mod.re_calc',
  "Region"
)

clean_data_client <- clean_data

clean_data_client$data <- clean_data$data %>% select(-any_of(c(extra_cols_data, qa_cols)))
clean_data_client$rpt_bio_children <- clean_data$rpt_bio_children %>% select(-any_of(c(extra_cols_child, qa_cols)))


raw_data_client <- raw_data
raw_data_client$data <- raw_data$data %>% select(-any_of(c(extra_cols_data, qa_cols)))
raw_data_client$rpt_bio_children <- raw_data$rpt_bio_children %>% select(-any_of(c(extra_cols_child, qa_cols)))

# remove extra objects from environment  
remove(list = c("qa_cols"))

# source("R/remove_extra_columns_value.R")
