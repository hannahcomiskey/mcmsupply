#' Clean the raw input DHS data so it is suitable to be used in the jags model.
#' Primarily this function addresses missing observations and standard errors (SE), replaces extremely tiny SE with reasonable SE
#' @name set_up_jags_data
#' @param mydata The DHS data SE_source_data_20.RDS from /data folder. Contains data for countries participating in the FP2030 initiative.
#' @return returns a cleaned DHS dataset
#' @export

set_up_jags_data <- function(mydata) {
  FP_source_data_wide <- mydata %>% # Proportion data
    dplyr::ungroup() %>%
    dplyr::select(Country, Region, Method, average_year, sector_category, prop.trans, n) %>%
    dplyr::rename(proportion = prop.trans) %>%
    dplyr::group_by(Country, Region, Method, average_year, sector_category) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = sector_category, values_from = c(proportion,n)) %>%
    dplyr::rename(Commercial_medical = proportion_Commercial_medical ) %>%
    dplyr::rename(Public = proportion_Public ) %>%
    dplyr::rename(Other = proportion_Other) %>%
    dplyr::arrange(Country)

  FP_source_data_wide <- FP_source_data_wide %>%
    dplyr::rowwise() %>%
    dplyr::mutate(check_total = sum(Commercial_medical, Other, Public, na.rm = TRUE))

  # Fill in single missing NA values with 1-sum(others) ----------------------------------------------------
  FP_source_data_wide$count_NA <- rowSums(is.na(FP_source_data_wide[, c("Commercial_medical", "Other","Public")])) # count NAs
  FP_source_data_wide <- FP_source_data_wide %>% dplyr::filter(count_NA <2) # Remove obs with two missing sectors
  FP_source_data_wide$remainder <- 1 - rowSums(FP_source_data_wide[, c("Commercial_medical", "Other","Public")], na.rm = TRUE)

  for(i in 1:nrow(FP_source_data_wide)) {
    if(FP_source_data_wide$count_NA[i]==1) {
      na_col_num <- which(is.na(FP_source_data_wide[i,c("Commercial_medical", "Other","Public")])) # column number of NA
      FP_source_data_wide[i,na_col_num+5] <- FP_source_data_wide[i,"remainder"] # replace with remainder
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

  ## Remove SE missing for two sectors -------------------------------------------------------------------
  SE_source_data_wide <- mydata %>% # SE data
    dplyr::ungroup() %>%
    dplyr::select(Country, Region, Method, average_year, sector_category, SE.proportion) %>%
    tidyr::pivot_wider(names_from = sector_category, values_from = SE.proportion) %>%
    dplyr::arrange(Country) %>%
    dplyr::rename(Public.SE = Public, Commercial_medical.SE = Commercial_medical, Other.SE = Other)

  # Readjust any SE <1% to 1% -------------------------------------------------------------------
  SE_source_data_wide <- SE_source_data_wide %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Public.SE = ifelse(Public.SE < 0.01, 0.01, Public.SE)) %>%
    dplyr::mutate(Commercial_medical.SE = ifelse(Commercial_medical.SE < 0.01, 0.01, Commercial_medical.SE)) %>%
    dplyr::mutate(Other.SE = ifelse(Other.SE < 0.01, 0.01, Other.SE))

  SE_source_data_wide$count_NA <- rowSums(is.na(SE_source_data_wide)) # count NAs
  SE_source_data_wide <- SE_source_data_wide %>% dplyr::filter(count_NA <2) # Remove obs with two missing sectors

  # Max of column SE in country for missing values -----------------------------------------------------
  SE_source_data_wide <- SE_source_data_wide %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Country) %>%
    dplyr::mutate(max.Other.SE = max(Other.SE, na.rm=TRUE)) %>%
    dplyr::mutate(Other.SE = ifelse(is.na(Other.SE)==TRUE, max.Other.SE, Other.SE)) %>%
    dplyr::mutate(max.Public.SE = max(Public.SE, na.rm=TRUE)) %>%
    dplyr::mutate(Public.SE = ifelse(is.na(Public.SE)==TRUE, max.Public.SE, Public.SE)) %>%
    dplyr::mutate(max.Commercial_medical.SE = max(Commercial_medical.SE, na.rm=TRUE)) %>%
    dplyr::mutate(Commercial_medical.SE = ifelse(is.na(Commercial_medical.SE)==TRUE, max.Commercial_medical.SE, Commercial_medical.SE)) %>%
    dplyr::select(Country, Region, Method, average_year, Commercial_medical.SE, Public.SE, Other.SE, count_NA)

  # Merge SE and proportion data together -----------------------------------------------------
  FP_source_data_wide <- dplyr::left_join(FP_source_data_wide, SE_source_data_wide)

  return(FP_source_data_wide)
}
