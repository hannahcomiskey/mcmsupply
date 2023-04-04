#' Get the country-specific median and precision terms for the alpha intercept in local national-level model runs.
#' @name get_national_local_parameters
#' @param mycountry The name of country of interest. Default is NULL. For the names of potential countries, review vigentte.
#' @param fp2030 Default to be TRUE. The country of interest is named as one of the Family Planning 2030 focus countries discussed in the Comiskey et al. paper.
#' @return A list of local parameters to be used to inform the intercept parameter alpha and the global variance-covariance matrix used in the wishart prior of the local_model_run.txt file.
#' @export

get_national_local_parameters <- function(mycountry=NULL, fp2030=TRUE) {

  # Read in national alpha estimates ---------------------------------
  median_alpha_region_intercepts <- mcmsupply::median_alpha_region_intercepts # read in regional-level (alpha_rms) median estimates
  precision_alpha_country_intercepts <- mcmsupply::precision_alpha_country_intercepts # read in country-level (alpha_cms) precision estimates
  Bspline_sigma_matrix_median <- mcmsupply::Bspline_sigma_matrix_median # read in national level correlations
  mydata <- get_national_data(fp2030=fp2030) # Read complete data set in without filtering for any country

  # Match regional intercepts to country names  ------------------------
  region_country_match <- mydata %>%
    select(Country, Super_region) %>%
    distinct() %>%
    filter(Country==mycountry) # List of matching countries to super-regions

  mydata <- superregion_index_fun(mydata, unique(mydata$Super_region))
  region_index_table <- tibble(Super_region = unique(mydata$Super_region), index_superregion = unique(mydata$index_superregion))
  dimnames(median_alpha_region_intercepts)[[3]] <- as.list(unlist(region_index_table$Super_region)) # Apply region names to parameter estimates
  myalpha_med <- median_alpha_region_intercepts[,,region_country_match$Super_region] # Take out relevant region

  return(list(alphahat_region = myalpha_med,
              tau_alphahat_cms = precision_alpha_country_intercepts,
              natRmat = Bspline_sigma_matrix_median))
}
