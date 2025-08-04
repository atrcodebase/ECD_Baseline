### attach value labels
# source("R/functions/labeler_function.R")

# Cloning df
clean_data_ulabeled <- clean_data

path = "./input/tool/ECD+Baseline+Tool.xlsx"

# Tool 0 ------------------------------------------------------------------
for(sheet in names(clean_data)){
  clean_data[[sheet]] <- labeler(
    data = clean_data[[sheet]],
    tool = path,
    survey_label = "label",
    choice_lable = "label"
  )
}


# remove extra objects from environment  
rm(path)
