#' Get JAGS model inputs
#' @param fp2030 TRUE/FALSE. Default is TRUE. Filters the data to only include FP2030 countries.
#' @param raw_subnatdata The subnational family planning source data from the 'get_subnational_data' function.
#' @return A dataset with the subnational regions names cleaned for Rwanda, Nigeria and Cote d'Ivoire. Optional to filter for only FP2030 countries.
#' @export
#' @examples Include only FP2030 countries: subnat_data <- clean_subnat_names(fp2030=TRUE, raw_subnatdata)
#' Include all countries: subnat_data <- clean_subnat_names(fp2030=FALSE, raw_subnatdata). Note this functionality is untested.

clean_subnat_names <- function(fp2030=TRUE, raw_subnatdata) {
  if(fp2030==TRUE) {
    FP_2030_countries <- c("Afghanistan","Benin","Burkina Faso","Cameroon",
                           "Congo", "Congo Democratic Republic", "Cote d'Ivoire",
                           "Ethiopia", "Ghana","Guinea","India","Kenya", "Liberia", "Madagascar",
                           "Malawi","Mali", "Mozambique", "Myanmar", "Nepal", "Niger", "Nigeria", "Pakistan",
                           "Philippines", "Rwanda", "Senegal", "Sierra Leone", "Togo", "Tanzania", "Uganda", "Zimbabwe")   # Using only FPET countries for now

    raw_subnatdata <- raw_subnatdata %>%
      dplyr::filter(Country %in% FP_2030_countries)
  }

  load("data/Country_and_area_classification_inclFP2020.rda")

  area_classification <- Country_and_area_classification_inclFP2020 %>%
    dplyr::select(`Country or area`, Region) %>%
    dplyr::rename(Country = `Country or area`) %>%
    dplyr::rename(Super_region = Region)

  FP_source_data_wide <- raw_subnatdata %>%
    dplyr::left_join(area_classification)

  FP_source_subset <- FP_source_data_wide %>%   # Replace issues with Rwanda names
    dplyr::filter(Country=="Rwanda") %>%
    dplyr::mutate(Region = dplyr::case_when(Region == "Ouest" ~ "West",
                              Region == "Sud" ~ "South",
                              Region == "Est" ~ "East",
                              Region == "Ville de Kigali" ~ "Kigali",
                              Region == "Kigali City" ~ "Kigali",
                              TRUE ~ as.character(Region))) %>%
    dplyr::filter(average_year > 2008) # removes old regions

  FP_source_data_wide <- FP_source_data_wide %>%
    dplyr::filter(Country!="Rwanda")

  FP_source_data_wide <- FP_source_data_wide %>%
    merge(FP_source_subset, all = TRUE)

  # Replace issues with Nigeria names
  FP_source_subset <- FP_source_data_wide %>%
    dplyr::filter(Country=="Nigeria") %>%
    dplyr::mutate(Region = dplyr::case_when(Region == "Northeast" ~ "North East",
                              Region == "Northwest" ~ "North West",
                              Region == "Southeast" ~ "South East",
                              Region == "Southwest" ~ "South West",
                              TRUE ~ as.character(Region)))

  FP_source_data_wide <- FP_source_data_wide %>%
    dplyr::filter(Country!="Nigeria")

  FP_source_data_wide <- FP_source_data_wide %>%
    merge(FP_source_subset, all = TRUE)

  # Replace issues with Cameroon names
  FP_source_subset <- FP_source_data_wide %>%
    dplyr::filter(Country=="Cote d'Ivoire") %>%
    dplyr::mutate(Region = dplyr::case_when(Region == "Center-East" ~ "Center East",
                              Region == "Center-North" ~ "Center North",
                              Region == "Center-West" ~ "Center West",
                              Region == "Center-South" ~ "Center South",
                              TRUE ~ as.character(Region)))

  FP_source_data_wide <- FP_source_data_wide %>%
    dplyr::filter(Country!="Cote d'Ivoire")

  FP_source_data_wide <- FP_source_data_wide %>%
    merge(FP_source_subset, all = TRUE)

  return(FP_source_data_wide)
}
