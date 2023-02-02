#' Get the country-specific median and precision terms for the alpha intercept in local model runs.
#' @name get_local_parameters
#' @param mycountry The name of country of interest. Default is NULL. For the names of potential countries, review vigentte.
#' @param fp2030 Default to be TRUE. The country of interest is named as one of the Family Planning 2030 focus countries discussed in the Comiskey et al. paper.
#' @return A list of local parameters to be used to inform the intercept parameter alpha and the global variance-covariance matrix used in the wishart prior of the local_model_run.txt file.
#' @export

get_local_parameters <- function(mycountry, fp2030=TRUE) {
  # Read in national alpha estimates ---------------------------------
  load("data/median_alpha_region_intercepts.rda")
  load("data/precision_alpha_country_intercepts.rda")
  load("data/Bspline_sigma_matrix_median.rda")

  mydata <- get_data(fp2030=fp2030) # Read complete data set in without filtering for any country
  mydata <- set_up_jags_data(mydata, mycountry=NULL)

  # Match regional intercepts to country names  ------------------------
  region_country_match <- mydata %>%
    select(Country, Region) %>%
    distinct() %>%
    filter(Country==mycountry) # List of matching countries to super-regions

  mydata <- region_index_fun(mydata, unique(mydata$Region))
  region_index_table <- tibble(Region = unique(mydata$Region), index_region = unique(mydata$index_region))
  dimnames(median_alpha_region_intercepts)[[3]] <- as.list(unlist(region_index_table$Region)) # Apply region names to parameter estimates
  myalpha_med <- median_alpha_region_intercepts[,,region_country_match$Region] # Take out relevant region

  return(list(alphahat_region = myalpha_med,
              tau_alphahat_cms = precision_alpha_country_intercepts,
              natRmat = Bspline_sigma_matrix_median))
}
