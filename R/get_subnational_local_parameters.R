#' Get the country-specific median and precision terms for the alpha intercept in local national-level model runs.
#' @name get_subnational_local_parameters
#' @param mycountry The name of country of interest. Default is NULL. For the names of potential countries, review vigentte.
#' @return A list of local parameters to be used to inform the intercept parameter alpha and the global variance-covariance matrix used in the wishart prior of the local_model_run.txt file.
#' @export

get_subnational_local_parameters <- function(mycountry, spatial=FALSE) {

  if(spatial==FALSE) {
    load("data/nonspatial_sigma_matrix_subnationalmod.rda") # non-spatial variance covariance matrix
  } else {
    load("data/spatial_sigma_matrix_subnationalmod.rda") # spatial variance covariance matrix
    sigma_delta_hat <- spatial_sigma_matrix_subnationalmod
  }
  # Read in subnational local parameters
  load("data/median_alphacms_subnationalmod.rda")
  load("data/tau_alphapms_subnationalmod.rda")

  myalpha_med <- median_alphacms[,,mycountry] # Take out relevant country

  return(list(alpha_cms = myalpha_med,
              tau_alphapms = tau_alpha_pms_hat,
              natRmat = sigma_delta_hat))
}
