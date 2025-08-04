source("./R/functions/convert_numbers_to_date.R")


convert_dates <- function(df, date_cols) {
  for (col in date_cols) {
    # Remove timezone name in parentheses
    cleaned <- sub(" \\(.*\\)", "", df[[col]])
    
    # Parse the cleaned date string
    parsed <- strptime(cleaned, "%a %b %d %Y %H:%M:%S GMT%z")
    
    # Format to "YYYY-MM-DD"
    df[[col]] <- format(parsed, "%Y-%m-%d")
  }
  return(df)
}


date_time_cols1 <- c("Starttime", "Endtime")
date_time_cols2 <- c("SubmissionDate")

clean_data$data <- convert_dates(clean_data$data, date_time_cols1)
clean_data$data <- convert_to_date_time(clean_data$data, date_time_cols2)
