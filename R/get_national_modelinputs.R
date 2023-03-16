#' Get JAGS model inputs
#' @name get_national_modelinputs
#' @param fp2030 TRUE/FALSE. Default is TRUE. Filters the data to only include FP2030 countries.
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @param startyear The year you wish to begin your predictions from. Default is 1990.
#' @param endyear The year you wish to finish your predictions. Default is 2030.5.
#' @param nsegments The number of knots you wish to use in your basis functions. Default is 12.
#' @param raw_data The national family planning source data from the 'get_subnational_data' function.
#'
#' @return A list of modelling inputs for the JAGS model.
#' 1. Tstar is the year index for the most recent survey in each province.
#' 2. Kstar is the knot index that aligns with Tstar.
#' 3. B.ik are the basis functions.
#' 4. n_years are total number of years
#' 5. n_obs  are the total number of observations
#' 6. K are the number of knots.
#' 7. H is K-1. Used in the calculation of first order differences of spline coefficients.
#' 8. M_count is the number of modern contraceptive methods.
#' 9. matchcountry is the country indexing to match the observed data to the predictions.
#' 10. matchmethod is the method indexing to match the observed data to the predictions.
#' 11. matchyears is the year indexing to match the observed data to the predictions.
#' @export
#'
#' @examples jagsdata <- get_modelinputs("Nepal", startyear=1990, endyear=2030.5, nsegments=12, mydata)
get_national_modelinputs <- function(fp2030=TRUE, local=FALSE, mycountry=NULL, startyear=1990, endyear=2030.5, nsegments=12, raw_data) {

  clean_FPsource <- standard_method_names(raw_data) # Standardizing method names

  n_method <- c("Female Sterilization", "Implants", "Injectables", "IUD", "OC Pills" ) # As per the method correlation matrix
  n_country <- unique(clean_FPsource$Country)
  n_superregion <- unique(clean_FPsource$Super_region)
  clean_FPsource <- country_index_fun(clean_FPsource, n_country)
  clean_FPsource <- method_index_fun(clean_FPsource, n_method)
  clean_FPsource <- superregion_index_fun(clean_FPsource, n_superregion)

  all_years <- seq(from = startyear, to = endyear, by=0.5) # 6-monthly increments
  n_all_years <- length(all_years)

  clean_FPsource <- clean_FPsource %>%
    dplyr::mutate(index_year = match(average_year,all_years)) # Time indexing - important for splines

  T_star <- clean_FPsource %>%
    dplyr::group_by(Country) %>%
    dplyr::filter(index_year==max(index_year)) %>%
    dplyr::select(Country, index_country, average_year, index_year) %>%
    dplyr::arrange(index_country) %>%
    dplyr::ungroup() %>%
    dplyr::select(index_country, index_year, average_year) %>%
    dplyr::distinct()

  nseg=nsegments   # Default is 12
  Kstar <- vector()
  B.ik <- array(dim = c(length(n_country), length(all_years),nseg+3))
  knots.all <- matrix(nrow = length(n_country), ncol=nseg+3)
  for(i in 1:nrow(T_star)) {
    index_mc <- T_star$average_year[i]
    res <- bs_bbase_precise(all_years, lastobs=index_mc, nseg = nseg)
    B.ik[i,,] <- res$B.ik
    Kstar[i] <- res$Kstar
    knots.all[i,] <- res$knots.k
  }

  K <- dim(res$B.ik)[2]
  H <- K-1
  if(local==TRUE) {
    B.ik <- B.ik[1,,] # change array to matrix for one country
  }

  t_seq_2 <- floor(clean_FPsource$index_year) # Time sequence for countries
  country_seq <- clean_FPsource$Country
  n_country <- as.character(unique(country_seq))
  n_region <- unique(clean_FPsource$Region)
  n_sector <- c("Public", "Commercial_medical", "Other") # Names of categories
  n_obs <- nrow(clean_FPsource) # Total number of observations
  year_seq <- seq(min(t_seq_2),max(t_seq_2), by=1)
  n_years <- length(year_seq) # number of years

  # Find the indexes of observations that match to the predicted responses
  match_country <- clean_FPsource$index_country
  match_years <- clean_FPsource$index_year
  match_method <- clean_FPsource$index_method
  region_country <- clean_FPsource %>%
    dplyr::select(Country, Super_region, index_country, index_superregion) %>%
    dplyr::distinct()
  match_superregion <- region_country$index_superregion

  return(list(data = clean_FPsource,
              tstar = T_star$index_year,
              kstar = Kstar,
              B.ik = B.ik,
              n_years = n_all_years,
              n_obs = n_obs,
              K = K,
              H = H,
              C_count = length(n_country),
              M_count = length(n_method),
              R_count = length(n_superregion),
              n_method = n_method, # As per the method correlation matrix
              n_country = n_country,
              n_super_region = n_superregion,
              all_years = all_years,
              matchsuperregion = match_superregion,
              matchcountry = match_country,
              matchmethod = match_method,
              matchyears = match_years)
         )
}
