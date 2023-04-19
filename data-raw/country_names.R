library(tibble)

country_names <- tibble::tribble(
  ~"Country names", ~"National level data available", ~"Subnational level data available",
  "Afghanistan", "Yes", "Yes",
  "Benin", "Yes", "Yes",
  "Burkina Faso", "Yes", "Yes",
  "Cameroon", "Yes", "Yes",
  "Congo", "Yes", "No",
  "Democratic Republic of Congo", "Yes", "Yes",
  "Cote d’Ivoire", "Yes", "Yes",
  "Ethiopia", "Yes", "Yes",
  "Ghana", "Yes", "Yes",
  "Guinea", "Yes", "Yes",
  "India", "Yes", "Yes",
  "Kenya", "Yes", "Yes",
  "Liberia", "Yes", "Yes",
  "Madagascar", "Yes", "Yes",
  "Malawi", "Yes", "Yes",
  "Mali", "Yes", "Yes",
  "Mozambique", "Yes", "Yes",
  "Myanmar", "Yes", "No",
  "Nepal", "Yes", "Yes",
  "Niger", "Yes", "Yes",
  "Nigeria", "Yes", "Yes",
  "Pakistan", "Yes", "Yes",
  "Philippines",  "Yes", "No",
  "Rwanda",  "Yes", "Yes",
  "Senegal",  "Yes", "Yes",
  "Sierra Leone", "Yes", "No",
  "Tanzania",  "Yes", "Yes",
  "Togo",  "Yes", "No",
  "Uganda",  "Yes", "Yes",
  "Zimbabwe",  "Yes", "Yes"
)

usethis::use_data(country_names, overwrite=TRUE)
saveRDS(country_names, file = "data-raw/country_names.RDS")
