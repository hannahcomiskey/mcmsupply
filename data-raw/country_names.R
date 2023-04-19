library(tibble)

country_names <- tibble::tribble(
  ~"Country names",
  "Afghanistan",
  "Benin",
  "Burkina Faso",
  "Cameroon",
  "Democratic Republic of Congo",
  "Cote d’Ivoire",
  "Ethiopia",
  "Ghana",
  "Guinea",
  "India",
  "Kenya",
  "Liberia",
  "Madagascar",
  "Malawi",
  "Mali",
  "Mozambique",
  "Nepal",
  "Niger",
  "Nigeria",
  "Pakistan",
  "Rwanda",
  "Senegal",
  "Tanzania",
  "Uganda",
  "Zimbabwe"
)

saveRDS(country_names, file = "data-raw/country_names.RDS")
