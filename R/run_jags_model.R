#' Run the jags model for estimating the proportion of modern contraceptive methods supplied by the public & private Sectors using a Bayesian hierarchical penalized spline model
#' @name run_jags_model
#' @param mydata The DHS data SE_source_data_20.RDS from /data folder. Contains data for countries participating in the FP2030 initiative.
#' @param nseg Ghe number of knots to use in the splines. Default is 12.#'
#' @return returns the jags model object
#' importFrom("stats", "cor", "filter", "lag")
#' @import R2jags runjags tidyverse tidybayes
#' @export

run_jags_model <- function(jagsdata, jagsparams) {
  mod <- jags.parallel(data=jagsdata,
                       parameters.to.save=jagsparams,
                       model.file = "model/Bspline_model_mvn_ratios_withSE_thetaw_ms_refalpha_betak.txt",
                       n.iter = 150000,
                       n.burnin = 10000,
                       n.thin = 70)

  saveRDS(mod, "results/mod_results.RDS")

  return(mod)
}
