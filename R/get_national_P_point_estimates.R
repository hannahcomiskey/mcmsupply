#' Get the r and z ratio point estimates from the separate chains of the JAGS model runs
#' R and Z are the intermediate parameters that are used to estimates the final proportions. See the model file for context.
#' @name get_national_P_point_estimates
#' @param main_path String. Path where you have set your model results to be saved to.
#' @param pkg_data Output of the `mcmsupplylocal::get_subnational_modelinputs()` function.
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @return returns the point estimates for the jags model object
#' @import R2jags runjags tidyverse tidybayes foreach doMC sf spdep geodata
#' @export

get_national_P_point_estimates <- function(main_path, pkg_data, local=FALSE, mycountry=NULL) {

  # Read in JAGS model
  if(local==FALSE) {
    mymod <- readRDS(paste0(main_path,"mod_global_national_results.RDS"))
  } else {
    mymod <- readRDS(paste0(main_path, "mod_",mycountry,"_national_results.RDS"))
  }

  # Get model inputs for cross matching to estimates
  n_country <- pkg_data$n_country
  n_method <- pkg_data$n_method
  n_sector <- c("Public", "Commercial_medical", "Other")
  n_all_years <- pkg_data$n_years
  mydata <- pkg_data$data
  all_years <- pkg_data$all_years
  K <- pkg_data$K
  B.ik <- pkg_data$B.ik

  # Creating index tables for reference ------------------------------------------------------------
  country_index_table <- tibble(Country = n_country, index_country = unique(mydata$index_country))
  method_index_table <- tibble(Method = n_method, index_method = 1:5)
  sector_index_table <- tibble(Sector = n_sector, index_sector = 1:3)
  year_index_table <- tibble(average_year = all_years,
                             index_year = 1:n_all_years,
                             floored_year = floor(all_years))

  P_point_estimates <- get_national_P_median_quantiles(country_index_table, method_index_table, sector_index_table, year_index_table, mymod, local=local)

  if(is.null(mycountry)==TRUE) {
    saveRDS(P_point_estimates, file=paste0(main_path,"P_point_national_estimates.RDS"))
  } else {
    saveRDS(P_point_estimates, file=paste0(main_path,mycountry,"_P_point_national_estimates.RDS"))
  }
  return(P_point_estimates)
}


