## Tool 1: -----
clean_data$data <- clean_data$data |>
  left_join(qa_sheet |> select(KEY, qa_log_status = qa_status), by = "KEY")

# sheet = "rpt_bio_children"
for(sheet in names(clean_data)[-1]){
  clean_data[[sheet]] <- clean_data[[sheet]] |>
    mutate(PARENT_KEY = as.character(PARENT_KEY)) |>
    left_join(select(clean_data$data, any_of(meta_cols), KEY, qa_log_status), by = c("PARENT_KEY" = "KEY")) |>
    select(any_of(meta_cols), everything())
}
