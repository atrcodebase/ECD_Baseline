library(atRfunctions)
library(dplyr)
library(readxl)
library(openxlsx)

# Read the tools ---------------------------------------------------------------
tool1 <- read_excel("./input/tool/ECD+Baseline+Tool.xlsx") %>%  mutate(name = trimws(name))

# Create the relevancy Files ---------------------------------------------------
relevancy_file1 <- create_relevancy_file(tool1, pull(tool1[which(endsWith(tool1$name, "_Translation") | endsWith(tool1$name, "_QA") | endsWith(tool1$name, "_caption") | endsWith(tool1$name, "_Caption") |
                                                                   endsWith(tool1$name, "_Caption")), "name"]))


# Export outputs ---------------------------------------------------------------
write.xlsx(relevancy_file1, "./input/relevancy_file/relevancy_file_tool1.xlsx")
