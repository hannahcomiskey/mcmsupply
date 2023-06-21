subnatSE_source_data <- readRDS("inst/data-raw/subnatSE_source_ipums_data.RDS")
subnatSE_source_data <- subnatSE_source_data %>% dplyr::filter(Country!="Kenya") # remove admin-1 level data for Kenya. No longer utilised.

kenya_data <- readRDS("inst/data-raw/kenya_admin2_FPsource.RDS") %>% # admin-2 level data for Kenya.
  dplyr::rename(Region = region,
                sector_categories = sector_category) %>%
  dplyr::select(!Method_collapse)

subnat_FPsource_data <- subnatSE_source_data %>%
  dplyr::ungroup() %>%
  dplyr::select(all_of(colnames(kenya_data))) %>%
  dplyr::bind_rows(kenya_data) %>%
  dplyr::filter(Country %in% c("Benin","Burkina Faso","Cameroon",
                        "Congo Democratic Republic", "Cote d'Ivoire", "Ethiopia",
                        "Ghana", "Guinea", "India", "Kenya", "Liberia", "Nepal",
                        "Philippines", "Rwanda", "Senegal", "Sierra Leone", "Zimbabwe")   # Using only a few countries for sample
  )

usethis::use_data(subnat_FPsource_data, overwrite = TRUE)