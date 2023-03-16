# This format list uses the data above. The format list is used to check user survey data.
library(dplyr)
data <- mcmsupply::subnat_FPsource_data

subnat_FPsource_format <- list(
  "Country" = list(
    "type" = "value",
    "valid" = data %>% dplyr::ungroup() %>% dplyr::select(Country) %>% unique() %>% unlist() %>% as.vector(),
    "missing" = FALSE,
    "required" = TRUE
  ),
  "Region" = list(
    "type" = "value",
    "valid" = data %>% dplyr::ungroup() %>% dplyr::select(Region) %>% unique() %>% unlist() %>% as.vector(),
    "missing" = FALSE,
    "required" = TRUE
  ),
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
  "sector_categories" = list(
    "type" = "value",
    "valid" = data %>% dplyr::ungroup() %>% dplyr::select(sector_categories) %>% unique() %>% unlist() %>% as.vector(),
    "missing" = FALSE,
    "required" = TRUE
  ),
  "proportion" = list(
    "type" = "range",
    "valid" = c(0, 1),
    "missing" = data$proportion %>% is.na() %>% any(), # check if any of the values are NA
    "required" = TRUE
  ),
  "SE.proportion" = list(
    "type" = "range",
    "valid" = c(0, 1),
    "missing" = data$SE.proportion %>% is.na() %>% any(),
    "required" = TRUE
  ),
  "n" = list(
    "basic" = TRUE,
    "required" = TRUE
  )
)

usethis::use_data(subnat_FPsource_format, overwrite = TRUE)
