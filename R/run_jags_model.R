#' Wrapper function to run the jags model for estimating the proportion of modern contraceptive methods supplied by the public & private Sectors using a Bayesian hierarchical penalized spline model for the national and subnational administration levels
#' @name run_jags_model
#' @param national TRUE/FALSE. Default is TRUE for national administration level data. FALSE retrieves subnational level data.
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
#' @examples National single-country example:
#' run_jags_model(jagsdata, jagsparams = NULL, main_path, n_iter = 80000, n_burnin = 10000, n_thin = 35, ...)
#'
#' National multi-country example:
#' run_jags_model(jagsdata, jagsparams = NULL, main_path, n_iter = 80000, n_burnin = 10000, n_thin = 35, ...)
#'
#' Subnational single-country example:
#' run_jags_model(jagsdata, jagsparams = NULL, main_path, n_iter = 80000, n_burnin = 10000, n_thin = 35, ...)
#'
#' Subnational multi-country example:
#' run_jags_model(jagsdata, jagsparams = NULL, main_path, n_iter = 80000, n_burnin = 10000, n_thin = 35, ...)
#'
#' To change from the default parameters monitored in the JAGS model:
#' myjagsparams <-c("P","alpha_pms")
#' run_jags_model(jagsdata, jagsparams = myjagsparams, main_path, n_iter = 80000, n_burnin = 10000, n_thin = 35, ...)
#' @export

run_jags_model <- function(jagsdata, jagsparams = NULL, main_path, n_iter = 80000, n_burnin = 10000, n_thin = 35, ...) {
  args <- jagsdata$args
  national <- args$national
  if(national==TRUE) {
    mod <- run_national_jags_model(jagsdata=jagsdata$modelinputs, jagsparams = jagsparams, local=args$local, main_path = main_path, n_iter = n_iter, n_burnin = n_burnin, n_thin = n_thin, mycountry=args$mycountry)
  } else {
    mod <- run_subnational_jags_model(jagsdata=jagsdata$modelinputs, jagsparams = jagsparams, local=args$local, main_path = main_path, n_iter = n_iter, n_burnin = n_burnin, n_thin = n_thin, mycountry=args$mycountry)
  }
  return(mod)
}
