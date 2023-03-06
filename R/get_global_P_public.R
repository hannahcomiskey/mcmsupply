#' Get posterior samples of public sector from r and z variables of JAGS model
#' @name get_subnational_global_P_public
#' @param main_path String. Path where you have set your model results to be saved to.
#' @return Saved samples for public supply shares.
#' @export

get_subnational_global_P_public <- function(main_path) {
  z <- readRDS(paste0(main_path, "zsamps.RDS"))
  P_public <- 1/(1+exp(-(z)))
  saveRDS(P_public, paste0(main_path,"P_public.RDS"))   ## Estimating all the Categories here (including total private)
  return(P_public = P_public)
}
