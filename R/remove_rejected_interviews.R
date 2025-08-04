
clean_data$data <- clean_data$data %>% filter(KEY %in% c(approved_keys) & !KEY %in% deleted_keys)
for(sheet in names(clean_data)[-1]){
  clean_data[[sheet]] <- clean_data[[sheet]] %>% filter(PARENT_KEY %in% clean_data$data$KEY & !KEY %in% deleted_keys)
}

