#' Get subnational family planning source data
#' @name get_subnational_data
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @return The input data for your country of interest, used as an input to the mcmsupply model
#' @export
#' @examples For an all-country-province dataset : mydata <- get_subnational_data(local=FALSE, mycountry=NULL)
#' For a one-country-province dataset: mydata <- get_subnational_data(local=TRUE, mycountry="Nepal")

get_subnational_data <- function(local=FALSE, mycountry=NULL) {
  load("data/subnatSE_source_data.rda")   # Read in SE data

  subnatSE_source_data <- subnatSE_source_data %>%
    dplyr::mutate(prop.trans = proportion*((nrow(subnatSE_source_data)-1)+0.33)/nrow(subnatSE_source_data)) %>%   # Y and SE transformation to account for (0,1) limits (total in sector)
    dplyr::filter(n>1) %>%
    dplyr::filter(SE.proportion!=0) %>%
    dplyr::filter(Region!="NA")

  FP_source_data_wide <- subnatSE_source_data %>% # Proportion data
    dplyr::ungroup() %>%
    dplyr::select(Country, Region, Method,  average_year, sector_categories, prop.trans, n) %>%
    dplyr::rename(proportion = prop.trans) %>% # USING TRANSFORMED DATA
    tidyr::pivot_wider(names_from = sector_categories, values_from = c(proportion,n)) %>% # separate data into columns for each sector
    dplyr::rename(Commercial_medical = proportion_Commercial_medical ) %>%
    dplyr::rename(Public = proportion_Public ) %>%
    dplyr::rename(Other = proportion_Other) %>%
    dplyr::arrange(Country)

  FP_source_data_wide <- FP_source_data_wide %>%
    dplyr::rowwise() %>%
    dplyr::mutate(check_total = sum(Commercial_medical, Other, Public, na.rm = TRUE)) # make sure proportions add to 1

  FP_source_data_wide$count_NA <- rowSums(is.na(FP_source_data_wide[, c("Other", "Public", "Commercial_medical")])) # count NAs
  FP_source_data_wide <- FP_source_data_wide %>%
    dplyr::filter(count_NA <2) # Remove obs with two missing sectors
  FP_source_data_wide$remainder <- 1 - rowSums(FP_source_data_wide[, c("Other", "Public", "Commercial_medical")], na.rm = TRUE)

  FP_source_data_wide <- FP_source_data_wide %>%
    dplyr::select(Country, Region, Method, average_year, Other, Public, Commercial_medical, n_Other, n_Public, n_Commercial_medical, check_total, count_NA, remainder)

  for(i in 1:nrow(FP_source_data_wide)) {   # Fill in single missing NA values with 1-sum(others)
    na_col_num <- which(is.na(FP_source_data_wide[i,c("Other", "Public", "Commercial_medical")])) # column number of NA
    if(FP_source_data_wide$count_NA[i]==1) {
      if(FP_source_data_wide$check_total[i]>0.9999) {
        FP_source_data_wide[i,na_col_num+4] <- 1-FP_source_data_wide$check_total[i]
      } else {
        na_col_num <- which(is.na(FP_source_data_wide[i,c("Other", "Public", "Commercial_medical")])) # column number of NA
        FP_source_data_wide[i,na_col_num+4] <- FP_source_data_wide[i,"remainder"] # replace with remainder
      }
    } else {
      next
    }
  }

  SE_source_data_wide <- subnatSE_source_data %>% #   ## Remove SE missing for two sectors
    dplyr::ungroup() %>%
    dplyr::select(Country, Region, Method,  average_year, sector_categories, SE.proportion) %>% # taking just SE not proportions
    tidyr::pivot_wider(names_from = sector_categories, values_from = SE.proportion) %>%
    dplyr::arrange(Country) %>%
    dplyr::rename(Public.SE = Public, Commercial_medical.SE = Commercial_medical, Other.SE = Other)

  SE_source_data_wide <- SE_source_data_wide %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Public.SE = ifelse(Public.SE < 0.01, 0.01, Public.SE)) %>%   # Readjust any SE <1% to 1%
    dplyr::mutate(Commercial_medical.SE = ifelse(Commercial_medical.SE < 0.01, 0.01, Commercial_medical.SE)) %>%
    dplyr::mutate(Other.SE = ifelse(Other.SE < 0.01, 0.01, Other.SE))

  SE_source_data_wide$count_NA <- rowSums(is.na(SE_source_data_wide)) # count NAs
  SE_source_data_wide <- SE_source_data_wide %>% dplyr::filter(count_NA <2) # Remove obs with two missing sectors

  SE_source_data_wide <- SE_source_data_wide %>%
    dplyr::group_by(Country, Region, Method, average_year) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Other.SE = ifelse(is.na(Other.SE)==TRUE | is.infinite(Other.SE)==TRUE, mean(Public.SE, Commercial_medical.SE), Other.SE)) %>% #   # Replace missing SE with average of other sectors
    dplyr::mutate(Public.SE = ifelse(is.na(Public.SE)==TRUE | is.infinite(Public.SE)==TRUE, mean(Other.SE, Commercial_medical.SE), Public.SE)) %>%
    dplyr::mutate(Commercial_medical.SE = ifelse(is.na(Commercial_medical.SE)==TRUE | is.infinite(Commercial_medical.SE)==TRUE, mean(Public.SE, Other.SE), Commercial_medical.SE)) %>%
    dplyr::select(Country, Region, Method, average_year, Commercial_medical.SE, Public.SE, Other.SE, count_NA)

  FP_source_data_wide <- dplyr::left_join(FP_source_data_wide, SE_source_data_wide)   # Merge SE and proportion data together

  mydata <- FP_source_data_wide %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Public = ifelse(Public < 0, 0.001, Public)) %>%   # Replace any negative numbers with approximately 0 (safety net)
    dplyr::mutate(Commercial_medical = ifelse(Commercial_medical < 0, 0.001, Commercial_medical)) %>%
    dplyr::mutate(Other = ifelse(Other < 0, 0.001, Other))

  if(local==TRUE & is.null(mycountry)==FALSE) {
    mydata <- FP_source_data_wide %>% dplyr::filter(Country==mycountry)

    if(mycountry == "Rwanda") { # Addressing issues with Rwanda subnational region names
      mydata <- mydata %>%
        dplyr::mutate(Region = dplyr::case_when(Region == "Ouest" ~ "West",
                                                Region == "Sud" ~ "South",
                                                Region == "Est" ~ "East",
                                                Region == "Ville de Kigali" ~ "Kigali",
                                                Region == "Kigali City" ~ "Kigali",
                                                TRUE ~ as.character(Region))) %>%
        dplyr::filter(average_year > 2008) # removes old regions
    }

    if(mycountry == "Nigeria") { # Addressing issues with Nigeria subnational region names
      mydata <- mydata %>%
        dplyr::mutate(Region = dplyr::case_when(Region == "Northeast" ~ "North East",
                                                Region == "Northwest" ~ "North West",
                                                Region == "Southeast" ~ "South East",
                                                Region == "Southwest" ~ "South West",
                                                TRUE ~ as.character(Region)))
    }

    if(mycountry == "Cote d'Ivoire") { # Addressing issues with Cote d'Ivoire subnational region names
      mydata <- mydata %>%
        dplyr::mutate(Region = dplyr::case_when(Region == "Center-East" ~ "Center East",
                                                Region == "Center-North" ~ "Center North",
                                                Region == "Center-West" ~ "Center West",
                                                Region == "Center-South" ~ "Center South",
                                                TRUE ~ as.character(Region)))
    }

    mydata <- mydata %>%
      dplyr::filter(n_Other >= 5 | n_Public >= 5 | n_Commercial_medical >= 5) %>% # Remove sample size less than 5, replace SE with max SE for region-method combo
      dplyr::mutate(Other.SE = ifelse(Other.SE < 0.01, 0.01, Other.SE)) %>%
      dplyr::mutate(Public.SE = ifelse(Public.SE < 0.01, 0.01, Public.SE)) %>%
      dplyr::mutate(Commercial_medical.SE = ifelse(Commercial_medical.SE < 0.01, 0.01, Commercial_medical.SE)) %>%
      dplyr::mutate(Country = as.factor(Country)) %>%
      droplevels() # remove factor levels of other countries
  }

  return(mydata)
}
