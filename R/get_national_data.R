#' Get the DHS data used for modelling the proportion of modern contraceptives supplied by the public and private sectors at the national level.
#' @name get_national_data
#' @param local TRUE/FALSE. Default is FALSE for global runs. Decides if this is a single-country or global run.
#' @param mycountry The name of country of interest. Default is NULL. For the names of potential countries, review vigentte.
#' @param fp2030=TRUE Filter raw data to only include the Family Planning 2030 focus countries discussed in the Comiskey et al. paper.
#' @param surveydata_filepath Path to survey data. Default is NULL. Survey data should be a .xlsx with the following format \code{\link{national_FPsource_data}}.
#' @return returns the DHS data set used for inputs into the model
#' @export

get_national_data <- function(local=FALSE, mycountry=NULL, fp2030=TRUE, surveydata_filepath=NULL) {
  if(is.null(surveydata_filepath)==TRUE){
    print("Using preloaded data!")
    load("data/national_FPsource_data.rda") # Read in data
  } else {
    print(paste0("Using file from ", surveydata_filepath))
    national_FPsource_data <- readxl::read_xlsx(surveydata_filepath)
    load("data/national_FPsource_format.rda")
    check_format(national_FPsource_format, national_FPsource_data) # Check if user input data is suitable for inclusion
  }

  if(fp2030==TRUE) {
    FP_2030_countries <- c("Afghanistan","Benin","Burkina Faso","Cameroon",
                           "Congo", "Congo Democratic Republic", "Cote d'Ivoire",
                           "Ethiopia", "Ghana","Guinea","India","Kenya", "Liberia", "Madagascar",
                           "Malawi","Mali", "Mozambique", "Myanmar", "Nepal", "Niger", "Nigeria", "Pakistan",
                           "Philippines", "Rwanda", "Senegal", "Sierra Leone", "Togo", "Tanzania", "Uganda", "Zimbabwe")

    national_FPsource_data <- national_FPsource_data %>% dplyr::filter(Country %in% FP_2030_countries)
  }

  FP_source_data_wide <- national_FPsource_data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Country, Super_region, Method, average_year, sector_category) %>%
    dplyr::distinct() %>%
    dplyr::select(Country, Super_region, Method, average_year, sector_category, proportion, n) %>%
    tidyr::pivot_wider(names_from = sector_category, values_from = c(proportion,n)) %>%
    dplyr::rename(Commercial_medical = proportion_Commercial_medical ) %>%
    dplyr::rename(Public = proportion_Public ) %>%
    dplyr::rename(Other = proportion_Other) %>%
    dplyr::arrange(Country)

  FP_source_data_wide <- FP_source_data_wide %>%
    dplyr::rowwise() %>%
    dplyr::mutate(check_total = sum(Commercial_medical, Other, Public, na.rm = TRUE))

  # # Fill in single missing NA values with 1-sum(others) -------------------------
  FP_source_data_wide$count_NA <- rowSums(is.na(FP_source_data_wide[, c("Commercial_medical", "Other","Public")])) # count NAs
  FP_source_data_wide <- FP_source_data_wide %>% filter(count_NA <2) # Remove obs with two missing sectors
  FP_source_data_wide$remainder <- 1 - rowSums(FP_source_data_wide[, c("Commercial_medical", "Other","Public")], na.rm = TRUE)

  for(i in 1:nrow(FP_source_data_wide)) {
    if(FP_source_data_wide$count_NA[i]==1) {
      na_col_num <- which(is.na(FP_source_data_wide[i,c("Commercial_medical", "Other","Public")])) # column number of NA
      num <- which(colnames(FP_source_data_wide)=="Commercial_medical")
      FP_source_data_wide[i,na_col_num+(num-1)] <- FP_source_data_wide[i,"remainder"] # replace with remainder
    } else {
      next
    }
  }

  # Replace any negative numbers with approximately 0
  FP_source_data_wide <- FP_source_data_wide %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Public = ifelse(Public < 0, 0.001, Public)) %>%
    dplyr::mutate(Commercial_medical = ifelse(Commercial_medical < 0, 0.001, Commercial_medical)) %>%
    dplyr::mutate(Other = ifelse(Other < 0, 0.001, Other))

  ## Remove SE missing for two sectors ---------------------
  SE_source_data_wide <- national_FPsource_data %>% # SE data
    dplyr::ungroup() %>%
    dplyr::select(Country, Super_region, Method, average_year, sector_category, SE.proportion) %>%
    tidyr::pivot_wider(names_from = sector_category, values_from = SE.proportion) %>%
    dplyr::arrange(Country) %>%
    dplyr::rename(Public.SE = Public, Commercial_medical.SE = Commercial_medical, Other.SE = Other)

  # Readjust any SE <1% to 1%
  SE_source_data_wide <- SE_source_data_wide %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Public.SE = ifelse(Public.SE < 0.01, 0.01, Public.SE)) %>%
    dplyr::mutate(Commercial_medical.SE = ifelse(Commercial_medical.SE < 0.01, 0.01, Commercial_medical.SE)) %>%
    dplyr::mutate(Other.SE = ifelse(Other.SE < 0.01, 0.01, Other.SE))

  # Count NAs to drop observations with too much missing data
  SE_source_data_wide$count_NA <- rowSums(is.na(SE_source_data_wide)) # count NAs
  SE_source_data_wide <- SE_source_data_wide %>% filter(count_NA <2) # Remove obs with two missing sectors

  # Max of column SE in country for missing values
  SE_source_data_wide <- SE_source_data_wide %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Country) %>%
    dplyr::mutate(max.Other.SE = max(Other.SE, na.rm=TRUE)) %>%
    dplyr::mutate(Other.SE = ifelse(is.na(Other.SE)==TRUE, max.Other.SE, Other.SE)) %>%
    dplyr::mutate(max.Public.SE = max(Public.SE, na.rm=TRUE)) %>%
    dplyr::mutate(Public.SE = ifelse(is.na(Public.SE)==TRUE, max.Public.SE, Public.SE)) %>%
    dplyr::mutate(max.Commercial_medical.SE = max(Commercial_medical.SE, na.rm=TRUE)) %>%
    dplyr::mutate(Commercial_medical.SE = ifelse(is.na(Commercial_medical.SE)==TRUE, max.Commercial_medical.SE, Commercial_medical.SE)) %>%
    dplyr::select(Country, Super_region, Method, average_year, Commercial_medical.SE, Public.SE, Other.SE, count_NA)

  # Merge SE and proportion data together
  FP_source_data_wide <- left_join(FP_source_data_wide, SE_source_data_wide) %>%
    dplyr::arrange(Country, Super_region, Method, average_year)

  if(local==TRUE & is.null(mycountry)==FALSE) { # Subset data for country of interest ---------------------------
    print(paste0("Getting data for ",mycountry))
    FP_source_data_wide <- FP_source_data_wide %>% filter(Country==mycountry)
  }

  return(FP_source_data_wide)
}
