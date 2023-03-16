#' Get posterior samples of other sector from r and z variables of JAGS model
#' @name get_subnational_global_P_other
#' @param main_path String. Path where you have set your model results to be saved to.
#' @return Saved samples for other supply shares.
#' @export

get_subnational_global_P_other <- function(main_path) {
  P_public <- readRDS(paste0(main_path,"P_public.RDS"))   ## Estimating all the Categories here (including total private)
  P_CM <- readRDS(paste0(main_path,"P_CM.RDS"))
  P_other <- (1-P_public) - P_CM
  saveRDS(P_other, paste0(main_path,"P_other.RDS"))
  return(P_other = P_other)
}
