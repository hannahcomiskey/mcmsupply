#' Run the jags model for estimating the proportion of modern contraceptive methods supplied by the public & private Sectors using a Bayesian hierarchical penalized spline model
#' @name run_national_jags_model
#' @param jagsdata The inputs for the JAGS model
#' @param jagsparams The parameters of the JAGS model you wish to review
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param main_path Default is "results/". String to indicate where to save results.
#' @param n_iter Default is 80000. Number of itterations to do in JAGS model.
#' @param n_burnin Default is 10000. Number of samples to burn-in in JAGS model.
#' @param n_thin Default is 35. Number of samples to thin by in JAGS model.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @return returns the jags model object
#' importFrom("stats", "cor", "filter", "lag")
#' @import R2jags runjags tidyverse tidybayes
#' @export

run_national_jags_model <- function(jagsdata, jagsparams, local=FALSE, main_path,
                                    n_iter = 80000, n_burnin = 10000, n_thin = 35, mycountry=NULL) {

  print("Saving results to the following pathway:")
  print(main_path)

  if(local==TRUE & is.null(mycountry)==FALSE) {
    mod <- jags.parallel(data=jagsdata,
                         parameters.to.save=jagsparams,
                         model.file = "model/local_national_model.txt",
                         n.burnin = n_burnin,
                         n.iter = n_iter,
                         n.thin = n_thin)
    saveRDS(mod, paste0(main_path, "mod_",mycountry,"_national_results.RDS"))
  } else {
    mod <- jags.parallel(data=jagsdata,
                         parameters.to.save=jagsparams,
                         model.file = "model/global_national_model.txt",
                         n.burnin = n_burnin,
                         n.iter = n_iter,
                         n.thin = n_thin)
    saveRDS(mod, paste0(main_path, "mod_global_national_results.RDS"))
  }
  return(mod)
}
