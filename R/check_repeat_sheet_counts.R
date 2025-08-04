source("./R/functions/compare_row_counts.R")


# Changed the supposed_row_count value from COUNT column in data sets to column define in the tool repeat-count - 131223
repeat_sheet_issues <- compare_row_counts(
      supposed_count_df = select(clean_data$data, supposed_row_count = how_many_children_adolescents, KEY),
      child_df = clean_data$rpt_bio_children,
      child_sheet_name = "rpt_bio_children"
    ) |> mutate(Row_count_from_tab = "data", Row_count_column_name = "how_many_children_adolescents")

# Remove Unnecessary Objects from Environment --------------------------------------------------------------
rm(list = c("count_sm_selected_choices", "compare_row_counts"))
