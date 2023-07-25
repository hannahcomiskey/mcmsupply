# Read in database

DEFT_DHS_database <- readxl::read_xlsx("inst/data-raw/DEFT_DHS_database.xlsx") %>%
  dplyr::rename(average_year = Year)

usethis::use_data(DEFT_DHS_database, overwrite=TRUE)
