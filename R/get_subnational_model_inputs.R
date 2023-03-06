#' Get JAGS model inputs
#' @name get_subnational_modelinputs
#' @param fp2030 TRUE/FALSE. Default is TRUE. Filters the data to only include FP2030 countries.
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param spatial TRUE/FALSE. Default is FALSE. spatial=FALSE retrieves the data for all subnational provinces across all countries without GPS information. spatial=TRUE retrieves for data for countries with GPS information as well as FP source data.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @param startyear The year you wish to begin your predictions from. Default is 1990.
#' @param endyear The year you wish to finish your predictions. Default is 2030.5.
#' @param nsegments The number of knots you wish to use in your basis functions. Default is 12.
#' @param raw_subnatdata The subnational family planning source data from the 'get_subnational_data' function.
#'
#' @return A list of modelling inputs for the JAGS model.
#' 1. Tstar is the year index for the most recent survey in each province.
#' 2. Kstar is the knot index that aligns with Tstar.
#' 3. B.ik are the basis functions.
#' 4. n_years are total number of years
#' 5. n_obs  are the total number of observations
#' 6. K are the number of knots.
#' 7. H is K-1. Used in the calculation of first order differences of spline coefficients.
#' 8. P_count is the number of subnational provinces/regions.
#' 9. M_count is the number of modern contraceptive methods.
#' 10. matchsubnat is the subnational province indexing to match the observed data to the predictions.
#' 11. matchcountry is the country indexing to match the observed data to the predictions.
#' 12. matchmethod is the method indexing to match the observed data to the predictions.
#' 13. matchyears is the year indexing to match the observed data to the predictions.
#' @export
#'
#' @examples jagsdata <- get_modelinputs("Nepal", startyear=1990, endyear=2030.5, nsegments=12, mydata)
get_subnational_modelinputs <- function(fp2030=TRUE, local=FALSE, spatial=FALSE, mycountry=NULL, startyear=1990, endyear=2030.5, nsegments=12, raw_subnatdata) {

  clean_FPsource <- clean_subnat_names(fp2030=fp2030, raw_subnatdata) # Clean subnational region names for Rwanda, Nigeria, Cote d'Ivoire

  if(local==TRUE & is.null(mycountry)==FALSE) {
    clean_FPsource <- clean_FPsource %>% dplyr::filter(Country==mycountry) # Subset data to country of interest
  }

  # Remove sample size less than 5, replace SE with max SE for region-method combo --------------------
  clean_FPsource <- clean_FPsource %>%
    dplyr::filter(n_Other >= 5 | n_Public >= 5 | n_Commercial_medical >= 5) %>%
    dplyr::mutate(Other.SE = ifelse(Other.SE < 0.01, 0.01, Other.SE)) %>%
    dplyr::mutate(Public.SE = ifelse(Public.SE < 0.01, 0.01, Public.SE)) %>%
    dplyr::mutate(Commercial_medical.SE = ifelse(Commercial_medical.SE < 0.01, 0.01, Commercial_medical.SE))

  if(spatial==TRUE){
    load("data/global_provincial_neighbouradj.rda")   # Filter for spatial countries: Not all countries have GPS data
    roworder <- rownames(global_provincial_neighbouradj)

    clean_FPsource <- clean_FPsource %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Country_region = paste0(Country, "_", Region)) %>%
      dplyr::filter(Country_region %in% roworder)
  }

  clean_FPsource <- standard_method_names(clean_FPsource) # Standardizing method names
  country_subnat_tbl <- clean_FPsource %>%
    dplyr::group_by(Country, Region) %>%
    dplyr::select(Country, Region) %>%
    dplyr::distinct() # table of country and regions (repeats in region names)
  n_method <- c("Female Sterilization", "Implants", "Injectables", "IUD", "OC Pills" ) # As per the method correlation matrix
  n_country <- unique(country_subnat_tbl$Country)
  n_subnat <- country_subnat_tbl$Region
  n_superregion <- unique(clean_FPsource$Super_region)

  clean_FPsource <- subnat_index_fun(clean_FPsource, n_subnat, country_subnat_tbl$Country) # Adding indexes to  data
  clean_FPsource <- country_index_fun(clean_FPsource, n_country)
  clean_FPsource <- method_index_fun(clean_FPsource, n_method)
  clean_FPsource <- superregion_index_fun(clean_FPsource, n_superregion)

  all_years <- seq(from = startyear, to = endyear, by=0.5) # 6-monthly increments
  n_all_years <- length(all_years)

  clean_FPsource <- clean_FPsource %>%
    dplyr::mutate(index_year = match(average_year,all_years)) # Time indexing - important for splines

  t_seq_2 <- floor(clean_FPsource$index_year) # Time sequence for countries
  country_seq <- clean_FPsource$Country
  n_country <- unique(country_subnat_tbl$Country)
  n_sector <- c("Public", "Commercial_medical", "Other") # Names of categories
  n_obs <- nrow(clean_FPsource) # Total number of observations
  year_seq <- seq(min(t_seq_2),max(t_seq_2), by=1)
  n_years <- length(year_seq) # Total number of years

  country_index_tbl <- clean_FPsource %>%
    dplyr::group_by(Country, index_country) %>%
    dplyr::select() %>%
    dplyr::distinct() # table of country and regions (repeats in region names)

  index_country_subnat_tbl <- clean_FPsource %>%
    dplyr::group_by(index_country, index_subnat) %>%
    dplyr::select(Region, index_subnat, Country, index_country) %>%
    dplyr::distinct() # table of country and regions (repeats in region names)

  index_superregion_tbl <- clean_FPsource %>%
    group_by(index_superregion, index_country) %>%
    dplyr::select(Country, index_country, Super_region, index_superregion) %>%
    distinct()

  match_superregion <- index_superregion_tbl$index_superregion

  match_country <- index_country_subnat_tbl$index_country

  match_years <- clean_FPsource$index_year # observation year indexes in the prediction years

  match_method <- clean_FPsource$index_method # method indexes from the data

  match_subnat <-  clean_FPsource$index_subnat #subnational region indexes from the data

  T_star <- clean_FPsource %>%
    dplyr::group_by(Country, Region) %>%
    dplyr::filter(index_year==max(index_year)) %>%
    dplyr::select(Country, Region, index_country, index_subnat, average_year, index_year) %>%
    dplyr::arrange(index_subnat) %>%
    dplyr::ungroup() %>%
    dplyr::select(index_country, index_subnat, average_year, index_year) %>%
    dplyr::distinct()

  nseg=nsegments   # Default is 12
  Kstar <-  vector()
  B.ik <- array(dim = c(length(n_subnat), length(all_years),nseg+3))
  knots.all <- matrix(nrow=length(n_subnat), ncol=nseg+3)

  for(i in 1:nrow(T_star)) {
    index_p <- T_star$index_subnat[i]
    index_msn <- T_star$average_year[i]
    res <- bs_bbase_precise(all_years, lastobs=index_msn, nseg = nseg) # number of splines based on segments, here choosing 10
    B.ik[index_p,,] <- res$B.ik
    Kstar[index_p] <- res$Kstar
    knots.all[index_p,] <- res$knots.k
  }

  K <- dim(res$B.ik)[2]
  H <- K-1

  return(list(data = clean_FPsource,
              tstar = T_star$index_year,
              kstar = Kstar,
              B.ik = B.ik,
              n_years = n_all_years,
              n_obs = n_obs,
              K = K,
              H = H,
              C_count = length(n_country),
              P_count = length(n_subnat),
              M_count = length(n_method),
              R_count = length(n_superregion),
              n_method = n_method, # As per the method correlation matrix
              n_country = n_country,
              n_subnat = n_subnat,
              n_super_region = n_superregion,
              all_years = all_years,
              matchsuperregion = match_superregion,
              matchsubnat = match_subnat,
              matchcountry = match_country,
              matchmethod = match_method,
              matchyears = match_years
  ))
}
