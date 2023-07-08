#' Wrapper function to run the jags model for estimating the proportion of modern contraceptive methods supplied by the public & private Sectors using a Bayesian hierarchical penalized spline model for the national and subnational administration levels
#' @name run_jags_model
#' @param jagsdata The object from the mcmsupply::get_modelinputs() function.
#' @param jagsparams The parameters of the JAGS model you wish to review
#' @param n_iter Default is 80000. Number of itterations to do in JAGS model.
#' @param n_burnin Default is 10000. Number of samples to burn-in in JAGS model.
#' @param n_thin Default is 35. Number of samples to thin by in JAGS model.
#' @param n_chain Default is 2. Number of chains to run in your MCMC sample.
#' @param n_cores The number of cores to use for parallel execution in subnational estimation. If not specified, the number of cores is set to the value of options("cores"), if specified, or to approximately half the number of cores detected by the parallel package.
#' @param ... Arguments from the mcmsupply::get_modelinputs() function.
#' @return returns the jags model object
#' @examples
#' \donttest{
#' raw_data <- get_data(national=FALSE, local=TRUE, mycountry="Nepal")
#' jagsdata <- get_modelinputs(startyear=1990, endyear=2025.5, nsegments=12, raw_data)
#' run_jags_model(jagsdata, n_iter=5, n_burnin=1, n_thin=1)
#' }
#' @export

run_jags_model <- function(jagsdata, jagsparams = NULL, n_iter = 80000, n_burnin = 10000, n_thin = 35, n_chain=2, n_cores=NULL, ...) {
  args <- jagsdata$args
  national <- args$national
  if(national==TRUE) {
    mod <- run_national_jags_model(jagsdata=jagsdata$modelinputs, jagsparams = jagsparams,
                                   local=args$local, n_iter = n_iter, n_burnin = n_burnin,
                                   n_thin = n_thin, n_chain=n_chain, mycountry=args$mycountry)
  } else {
    mod <- run_subnational_jags_model(jagsdata=jagsdata$modelinputs, jagsparams = jagsparams,
                                      local=args$local, n_iter = n_iter, n_burnin = n_burnin,
                                      n_thin = n_thin, n_chain=n_chain, mycountry=args$mycountry, n_cores=n_cores)
  }
  p <- suppressWarnings(get_point_estimates(jagsdata = jagsdata, n_chain=n_chain))  # Get point estimates with uncertainty for model output

  # Remove the temporary file and directory
  unlink(tempdir(), recursive = TRUE)

  return(list(JAGS = mod,
                estimates = p))
}
