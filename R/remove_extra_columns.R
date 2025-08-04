# getting the column names to exclude from data set for client ------------
qa_cols <- c("review_status", "review_quality", "review_comments", "review_corrections")
# DONE
extra_cols.tool9 <- c(
  # Meta Cols
  "Passcode", "deviceid", "subscriberid", "simid", "devicephonenum", "username", "duration", "device_info", "comments", "Surveyor_Name", "Province_filter",
  "Province_DariPashto", "District_DariPashto", "Village_DariPashto", "Date_And_Time", "Sector", "Line_Ministry_Name", 
  "Line_Ministry_Project_Id",  "Line_Ministry_SubProject_Id", "Line_Ministry_Sub_Project_Name_And_Description", "Type_Of_Implementing_Partner", "Type_Of_Site_Visit", 
  "Type_Of_Visit", "If_not_a_first_Site_Visit_state_Original_Site_Visit_ID",
  "Reporting_Period", "tpma_location_id", "Province_Climate", "Survey_Language", "Survey_Language_Other",
  "Sample_info_correct", "Village", "instanceID", "formdef_version", 
  'ID_Check', 'TPMA_Location_Name', 'Entity_Type',	'acknowledge', 'qa_log_status', 'AA_Full',# NEW
  'indx', 'CBE_Province',	'CBE_Province_DariPashto', 'CBE_Province_Pcode', 'CBE_District_DariPashto',	'CBE_District_Pcode',#NEW
  'CBE_Province_Climate',	'CBE_Village',	'CBE_Village_DariPashto', 'CBE_School_CBE_Name_DariPashto', 'D1_Hub_School_Name',
  
  
  
  # PII 
  "Respondent_Full_Name", "Respondent1_Phone_Number", "Respondent2_Phone_Number",
  
  # GPS
  "Geopoint1-Latitude",	"Geopoint1-Longitude",	"Geopoint1-Altitude",	"Geopoint1-Accuracy",
  'Geopoint2-Latitude',	'Geopoint2-Longitude',	'Geopoint2-Altitude',	'Geopoint2-Accuracy', # NEW
  "Geopoint0-Latitude",  "Geopoint0-Longitude", "Geopoint0-Altitude",  "Geopoint0-Accuracy",  "Geopoint3-Latitude",  "Geopoint3-Longitude", "Geopoint3-Altitude",  "Geopoint3-Accuracy", 
  "Geopoint4-Latitude",  "Geopoint4-Longitude", "Geopoint4-Altitude",  "Geopoint4-Accuracy",  "Geopoint5-Latitude",  "Geopoint5-Longitude", "Geopoint5-Altitude",  "Geopoint5-Accuracy",
  "Geopoint6-Latitude",  "Geopoint6-Longitude", "Geopoint6-Altitude",  "Geopoint6-Accuracy",
  
  
  # URL
  "text_audit_full",
  
  # Captions
  "A7_Photo1_Caption", "A7_Photo2_QA_Photo_Caption", "B2_Photo_Caption",
  'B6_Logbook_Photo_Caption', 'Please_Add_Any_Relevant_Photo_caption',#NEW
  'A7_Photo1_Caption',  'A7_Photo2_QA_Photo_Caption', 
  
  # Notes and Re-coded
  "Sample_Type",
  
  # repeat counter and SET-OFF
  "SET-OF-Relevant_photos",  "Questions_Repeat_count",	"SET-OF-Questions_Repeat",
  
  # Surveyor Comments
  "Surveyor_Comments",	"Surveyor_Comments_Translation"
)

# Tool 9
clean_data.tool9_for_client$data <- clean_data.tool9$data %>% select(-any_of(c(extra_cols.tool9, qa_cols)))

# sheet = "Relevant_photos"
for (sheet in names(clean_data.tool9_for_client)[-1]) {
  clean_data.tool9_for_client[[sheet]] <- clean_data.tool9_for_client[[sheet]] |> select(-any_of(c(extra_cols.tool9, qa_cols, "starttime"))) |>
    select(Site_Visit_ID, any_of("CBE_EMIS_School_ID_CBE_KEY"), everything())
}

# clean_data.tool9_for_client$Relevant_photos <- clean_data.tool9$Relevant_photos %>% mutate(PARENT_KEY = as.character(PARENT_KEY)) %>%  select(-any_of(c(extra_cols.tool9, qa_cols, "starttime"))) |>
#   select(Site_Visit_ID, IP_Name, everything())

# remove extra objects from environment  
remove(list = c("qa_cols"))

# source("R/remove_extra_columns_value.R")
