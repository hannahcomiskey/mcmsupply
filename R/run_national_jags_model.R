#' Run the jags model for estimating the proportion of modern contraceptive methods supplied by the public & private Sectors using a Bayesian hierarchical penalized spline model
#' @name run_national_jags_model
#' @param jagsdata The inputs for the JAGS model
#' @param jagsparams The parameters of the JAGS model you wish to review
#' @param local TRUE/FALSE. Default is FALSE for global runs. Decides if this is a single-country or global run.
#' @param mycountry The name of country of interest. Default is NULL. For the names of potential countries, review vigentte.
#' @return returns the jags model object
#' importFrom("stats", "cor", "filter", "lag")
#' @import R2jags runjags tidyverse tidybayes
#' @export

run_national_jags_model <- function(jagsdata, jagsparams, local=FALSE, mycountry=NULL) {
  if(local==TRUE & is.null(mycountry)==FALSE) {
    mod <- jags.parallel(data=jagsdata,
                         parameters.to.save=jagsparams,
                         model.file = "model/local_national_model.txt",
                         n.iter = 80000,
                         n.burnin = 10000,
                         n.thin = 35)
    saveRDS(mod, paste0("results/mod_",mycountry,"_results.RDS"))
  } else {
    mod <- jags.parallel(data=jagsdata,
                         parameters.to.save=jagsparams,
                         model.file = "model/global_national_model.txt",
                         n.iter = 150000,
                         n.burnin = 10000,
                         n.thin = 70)
    saveRDS(mod, "results/mod_global_results.RDS")
  }
  return(mod)
}
