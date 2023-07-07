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
  dplyr::filter(Country %in% c("Afghanistan","Benin","Burkina Faso","Cameroon",
                               "Congo", "Congo Democratic Republic", "Cote d'Ivoire",
                               "Ethiopia", "Ghana","Guinea","India","Kenya", "Liberia", "Madagascar",
                               "Malawi","Mali", "Mozambique", "Myanmar", "Nepal", "Niger", "Nigeria", "Pakistan",
                               "Philippines", "Rwanda", "Senegal", "Sierra Leone", "Togo", "Tanzania", "Uganda", "Zimbabwe")) %>%
  dplyr::mutate(Region = stringr::str_to_title(Region)) %>%
  dplyr::mutate(Region = stringr::str_replace_all(Region, "[[:punct:]]", " "))

FP_source_tmp <- subnat_FPsource_data %>%
  dplyr::filter(Country=="Rwanda") %>%
  dplyr::mutate(Region = dplyr::case_when(Region == "Ouest" ~ "West",
                                          Region == "Nord" ~ "North",
                                          Region == "Sud" ~ "South",
                                          Region == "Est" ~ "East",
                                          Region == "Ville de Kigali" ~ "Kigali",
                                          Region == "Kigali City" ~ "Kigali",
                                   TRUE ~ as.character(Region))) %>%
  dplyr::filter(average_year > 2008) # removes old regions

FP_source_data_wide <- subnat_FPsource_data %>%
  dplyr::filter(Country!="Rwanda")

FP_source_data_wide <- FP_source_data_wide %>%
  merge(FP_source_tmp, all = TRUE)

# Replace issues with Nigeria names
FP_source_tmp <- FP_source_data_wide %>%
  dplyr::filter(Country=="Nigeria") %>%
  dplyr::mutate(Region = dplyr::case_when(Region == "Northeast" ~ "North East",
                                   Region == "Northwest" ~ "North West",
                                   Region == "Southeast" ~ "South East",
                                   Region == "Southwest" ~ "South West",
                                   TRUE ~ as.character(Region)))

FP_source_data_wide <- FP_source_data_wide %>%
  dplyr::filter(Country!="Nigeria")

# Replace issues with Burkina Faso names
FP_source_tmp <- FP_source_data_wide %>%
  dplyr::filter(Country=="Burkina Faso") %>%
  dplyr::mutate(Region = dplyr::case_when(Region == "Nord" ~ "North",
                                          Region == "Est" ~ "East",
                                          Region == "Sud" ~ "South",
                                          Region == "Ouest" ~ "West",
                                          Region == "Centre-Sud" ~ "Central/South",
                                          Region == "Ouagadougou" ~ "Centre Including Ouagadougou",
                                   TRUE ~ as.character(Region)))

FP_source_data_wide <- FP_source_data_wide %>%
  dplyr::filter(Country!="Burkina Faso")

FP_source_data_wide <- FP_source_data_wide %>%
  merge(FP_source_tmp, all = TRUE)

# Replace issues with Cote d'Ivoire names
FP_source_tmp <- FP_source_data_wide %>%
  dplyr::filter(Country=="Cote d'Ivoire") %>%
  dplyr::mutate(Region = dplyr::case_when(Region == "Center-East" ~ "Center East",
                                   Region == "Center-North" ~ "Center North",
                                   Region == "Center-West" ~ "Center West",
                                   Region == "Center-South" ~ "Center South",
                                   TRUE ~ as.character(Region)))

FP_source_data_wide <- FP_source_data_wide %>%
  dplyr::filter(Country!="Cote d'Ivoire")

FP_source_data_wide <- FP_source_data_wide %>%
  merge(FP_source_tmp, all = TRUE)

subnat_FPsource_data <- FP_source_data_wide %>% dplyr::arrange(Country, Region, Method, average_year)

usethis::use_data(subnat_FPsource_data, overwrite = TRUE)
