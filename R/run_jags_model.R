#' Run the jags model for estimating the proportion of modern contraceptive methods supplied by the public & private Sectors using a Bayesian hierarchical penalized spline model
#' @name run_jags_model
#' @param jagsdata The inputs for the JAGS model
#' @param jagsparams The parameters of the JAGS model you wish to review
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
