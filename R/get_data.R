#' Get the DHS data used for modelling the proportion of modern contraceptives supplied by the public and private sectors.
#' @name get_data
#' @param fp2030=TRUE Filter raw data to only include the Family Planning 2030 focus countries discussed in the Comiskey et al. paper.
#' @return returns the DHS data set used for inputs into the model
#' @export

get_data <- function(fp2030=TRUE) {
  load("data/SE_source_data_20.rda")
  load("data/Country_and_area_classification_inclFP2020.rda")

  SE_source_data_20 <- SE_source_data_20 %>% dplyr::select(!c(Method_collapse, check_sum))
  area_classification <- Country_and_area_classification_inclFP2020 %>%
    dplyr::select(`Country or area`, `Region`) %>%
    dplyr::rename(Country = `Country or area`)

  # Y and SE transformation to account for (0,1) limits (total in sector)
  SE_source_data <- SE_source_data_20 %>%
    dplyr::left_join(area_classification) %>%
    dplyr::mutate(prop.trans = proportion*((nrow(SE_source_data_20)-1)+0.33333)/nrow(SE_source_data_20)) %>%
    dplyr::mutate(SE.prop.trans = SE.proportion*((nrow(SE_source_data_20)-1)+0.33333)/nrow(SE_source_data_20)) %>%
    dplyr::filter(SE.prop.trans != 0)

  # Adding missing country world regions
  SE_source_data <- SE_source_data %>%
    dplyr::mutate(Region = dplyr::case_when(Country=="Bolivia" ~ "South America",
                              Country=="Kyrgyz Republic" ~ "Central Asia",
                              Country=="Moldova" ~ "Eastern Europe",
                              TRUE ~ as.character(Region)))

  if(fp2030==TRUE) {
    FP_2030_countries <- c("Afghanistan","Benin","Burkina Faso","Cameroon",
                           "Congo", "Congo Democratic Republic", "Cote d'Ivoire",
                           "Ethiopia", "Ghana","Guinea","India","Kenya", "Liberia", "Madagascar",
                           "Malawi","Mali", "Mozambique", "Myanmar", "Nepal", "Niger", "Nigeria", "Pakistan",
                           "Philippines", "Rwanda", "Senegal", "Sierra Leone", "Togo", "Tanzania", "Uganda", "Zimbabwe")

    SE_source_data <- SE_source_data %>% dplyr::filter(Country %in% FP_2030_countries)
  }
  return(SE_source_data)
}
