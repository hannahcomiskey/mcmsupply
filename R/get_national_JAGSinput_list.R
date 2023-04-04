#' Combines the data sources to create one JAGS input list
#' @name get_national_JAGSinput_list
#' @param pkg_data The data list from the 'mcmsupply::get_national_modelinputs' function.
#' @param local TRUE/FALSE. Default is FALSE for global runs. Decides if this is a single-country or global run.
#' @param mycountry The name of country of interest. Default is NULL. For the names of potential countries, review vigentte.
#' @return returns a list ready for input into the JAGS model
#' @export
#'
get_national_JAGSinput_list <- function(pkg_data, local= FALSE,  mycountry=NULL) {
  if(local==TRUE & is.null(mycountry)==FALSE) {
    local_parms <- get_national_local_parameters(mycountry=mycountry) # Get parameters for local informative priors for national data
    jags_data <- list(y = pkg_data$data[,c("Public", "Commercial_medical")], # Combine all data into one list ready for JAGS
                     se_prop = pkg_data$data[,c("Public.SE", "Commercial_medical.SE")],
                     alphahat_region = local_parms$alphahat_region,
                     tau_alphahat_cms = local_parms$tau_alphahat_cms,
                     natRmat = local_parms$natRmat, # dwish on inverse
                     natdf = length(pkg_data$n_method)+3,
                     tstar = pkg_data$tstar,
                     kstar = pkg_data$kstar,
                     B.ik = pkg_data$B.ik,
                     n_years = pkg_data$n_years,
                     n_obs = pkg_data$n_obs,
                     K = pkg_data$K,
                     H = pkg_data$H,
                     M_count = pkg_data$M_count,
                     matchmethod = pkg_data$matchmethod,
                     matchyears = pkg_data$matchyears
    )
  } else {
    estimated_rho_matrix <- mcmsupply::estimated_national_correlations %>% # Get global correlations for national data
      dplyr::select(row, column, public_cor, private_cor)
    my_SE_rho_matrix <- estimated_rho_matrix %>%
      dplyr::select(public_cor, private_cor)
    jags_data <- list(y = pkg_data$data[,c("Public", "Commercial_medical")], # Combine all data into one list ready for JAGS
                      se_prop = pkg_data$data[,c("Public.SE", "Commercial_medical.SE")],
                      rho = my_SE_rho_matrix,
                      kstar = pkg_data$kstar,
                      B.ik = pkg_data$B.ik,
                      n_years = pkg_data$n_years,
                      n_obs = pkg_data$n_obs,
                      K = pkg_data$K,
                      H = pkg_data$H,
                      C_count = pkg_data$C_count,
                      R_count = pkg_data$R_count,
                      M_count = pkg_data$M_count,
                      matchcountry = pkg_data$matchcountry,
                      matchregion = pkg_data$matchsuperregion,
                      matchmethod = pkg_data$matchmethod,
                      matchyears = pkg_data$matchyears
    )
  }
  return(jags_data)
}
