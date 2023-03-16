#' Get posterior samples of commercial medical sector from r and z variables of JAGS model
#' @name get_subnational_global_P_CM
#' @param main_path String. Path where you have set your model results to be saved to.
#' @return Saved samples for commercial medical supply shares.
#' @export

get_subnational_global_P_CM <- function(main_path) {
  P_public <- readRDS(paste0(main_path,"P_public.RDS"))   ## Estimating all the Categories here (including total private)
  r <- readRDS(paste0(main_path, "rsamps.RDS"))
  P_CM <- (1/(1+exp(-(r))))*(1-P_public)
  saveRDS(P_CM, paste0(main_path,"P_CM.RDS"))
  return(P_CM = P_CM)
}
