#' Get posterior samples of P from r and z variables of JAGS model
#' @name get_subnational_global_P_samps
#' @param main_path String. Path where you have set your model results to be saved to.
#' @return Saved samples for public, commercial medical and other supply shares.
#' @export

get_subnational_global_P_samps <- function(main_path) {
  mcmsupplylocal::get_global_P_public(main_path)
  gc()
  mcmsupplylocal::get_subnational_global_P_CM(main_path)
  gc()
  mcmsupplylocal::get_subnational_global_P_other(main_path)
  gc()
}
