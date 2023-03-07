subnatSE_source_data <- readRDS("data-raw/subnatSE_source_ipums_data.RDS")
subnatSE_source_data <- subnatSE_source_data %>% filter(Country!="Kenya") # remove admin-1 level data for Kenya. No longer utilised.

kenya_data <- readRDS("data-raw/kenya_admin2_FPsource.RDS") %>% # admin-2 level data for Kenya.
  dplyr::rename(Region = region,
                sector_categories = sector_category) %>%
  dplyr::select(!Method_collapse)

subnatSE_source_data <- subnatSE_source_data %>%
  ungroup() %>%
  dplyr::select(all_of(colnames(kenya_data))) %>%
  bind_rows(kenya_data)

usethis::use_data(subnatSE_source_data)
