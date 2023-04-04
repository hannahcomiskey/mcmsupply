#' Get the country-specific median and precision terms for the alpha intercept in local national-level model runs.
#' @name get_subnational_local_parameters
#' @param mycountry The name of country of interest. Default is NULL. For the names of potential countries, review vigentte.
#' @return A list of local parameters to be used to inform the intercept parameter alpha and the global variance-covariance matrix used in the wishart prior of the local_model_run.txt file.
#' @export

get_subnational_local_parameters <- function(mycountry, spatial=FALSE) {

  if(spatial==FALSE) {
    sigma_delta_hat <- mcmsupply::sigma_delta_hat # non-spatial variance covariance matrix
  } else {
    sigma_delta_hat <- mcmsupply::spatial_sigma_matrix_subnationalmod # spatial variance covariance matrix
  }
  # Read in subnational local parameters
  median_alphacms <- mcmsupply::median_alphacms # load country-level intercept median estimates
  myalpha_med <- median_alphacms[,,mycountry] # Take out relevant country
  tau_alpha_pms_hat <- mcmsupply::tau_alpha_pms_hat # load subnational-level intercept precision
  return(list(alpha_cms = myalpha_med,
              tau_alphapms = tau_alpha_pms_hat,
              natRmat = sigma_delta_hat))
}
