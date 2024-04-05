# This format list uses the data above. The format list is used to check user survey data.
library(dplyr)
data <- mcmsupply::national_FPsource_data

national_FPsource_format <- list(
  "Country" = list(
    "basic" = TRUE, # check for data supplied (missing, range, NAs)
    "required" = TRUE, # check if it is required in the data
    "missing" = FALSE #check for missing values if they are not supported
  ),
  # "Super_region" = list(
  #   "type" = "value",
  #   #"valid" = data %>% dplyr::ungroup() %>% dplyr::select(Region) %>% unique() %>% unlist() %>% as.vector(),
  #   "missing" = FALSE,
  #   "required" = FALSE
  # ),
  "Method" = list(
    "type" = "value",
    "valid" = data %>% dplyr::ungroup() %>% dplyr::select(Method) %>% unique() %>% unlist() %>% as.vector(),
    "missing" = FALSE,
    "required" = TRUE
  ),
  "average_year" = list(
    "basic" = TRUE,
    "required" = TRUE
  ),
  # "sector_category" = list(
  #   "type" = "value",
  #   "valid" = data %>% dplyr::ungroup() %>% dplyr::select(sector_category) %>% unique() %>% unlist() %>% as.vector(),
  #   "missing" = FALSE,
  #   "required" = TRUE
  # ),
  "Public" = list(
    "type" = "range",
    "valid" = c(0, 1),
    "missing" = data$Public %>% is.na() %>% any(), # check if any of the values are NA
    "required" = TRUE
  ),
  "se.Public" = list(
    "type" = "range",
    "valid" = c(0, 1),
    "missing" = data$se.Public %>% is.na() %>% any(),
    "required" = TRUE
  ) ,
  "Commercial_medical" = list(
    "type" = "range",
    "valid" = c(0, 1),
    "missing" = data$Commercial_medical %>% is.na() %>% any(), # check if any of the values are NA
    "required" = TRUE
  ),
  "se.Commercial_medical" = list(
    "type" = "range",
    "valid" = c(0, 1),
    "missing" = data$se.Commercial_medical %>% is.na() %>% any(),
    "required" = TRUE
  ) ,
  "Other" = list(
    "type" = "range",
    "valid" = c(0, 1),
    "missing" = data$Other %>% is.na() %>% any(), # check if any of the values are NA
    "required" = TRUE
  ),
  # "se.Other" = list(
  #   "type" = "range",
  #   "valid" = c(0, 1),
  #   #"missing" = data$se.Other %>% is.na() %>% any(),
  #   "required" = FALSE
  # ) ,
  "Public_n" = list(
    "basic" = TRUE,
    "required" = TRUE
  ),
  "Commercial_medical_n" = list(
    "basic" = TRUE,
    "required" = TRUE
  ),
  "Other_n" = list(
    "basic" = TRUE,
    "required" = TRUE
  )
)

usethis::use_data(national_FPsource_format, overwrite = TRUE)
