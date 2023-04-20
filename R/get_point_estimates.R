#' Wrapper function to run the jags model for estimating the proportion of modern contraceptive methods supplied by the public & private Sectors using a Bayesian hierarchical penalized spline model for the national and subnational administration levels
#' @name get_point_estimates
#' @param main_path Default is "results/". String to indicate where to save results.
#' @param jagsdata Output of the `mcmsupply::get_modelinputs()` function.
#' @param ... Arguments from the `mcmsupply::get_modelinputs()` function.
#' @return returns the point estimates (median, 80% and 95% credible intervals) from the JAGS output
#' importFrom("stats", "cor", "filter", "lag")
#' @import R2jags runjags tidyverse tidybayes
#' @examples National single-country example:
#' get_point_estimates(main_path = "results/test", jagsdata)
#'
#' National multi-country example:
#' get_point_estimates(main_path = "results/test", jagsdata)
#'
#' Subnational single-country example:
#' get_point_estimates(main_path = "results/test", jagsdata)
#'
#' Subnational multi-country example:
#' get_point_estimates(main_path = "results/test", jagsdata)
#'
#' @export

get_point_estimates <- function(main_path=NULL, jagsdata, ...) {
  if(is.null(main_path)==TRUE) {
    main_path = "results/" # set default location for results
  }
  args <- jagsdata$args
  national <- args$national
  if(national==TRUE) {
    get_national_P_point_estimates(main_path, pkg_data = jagsdata$modelinputs, local=args$local, mycountry=args$mycountry)
  } else {
    get_subnational_P_point_estimates(main_path, pkg_data = jagsdata$modelinputs, local=args$local, mycountry=args$mycountry)
  }
  print("Results complete!")
}
