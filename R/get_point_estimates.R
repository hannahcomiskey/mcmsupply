#' Wrapper function to run the jags model for estimating the proportion of modern contraceptive methods supplied by the public & private Sectors using a Bayesian hierarchical penalized spline model for the national and subnational administration levels
#' @name get_point_estimates
#' @param national TRUE/FALSE. Default is TRUE for national administration level data. FALSE retrieves subnational level data.
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @param main_path Default is "results/". String to indicate where to save results.
#' @param pkg_data Output of the `mcmsupply::get_modelinputs()` function.
#' @return returns the point estimates (median, 80% and 95% credible intervals) from the JAGS output
#' importFrom("stats", "cor", "filter", "lag")
#' @import R2jags runjags tidyverse tidybayes
#' @examples National single-country example:
#' get_point_estimates(national=TRUE, local=TRUE, mycountry="Nepal", main_path = "results/test", pkg_data)
#'
#' National multi-country example:
#' get_point_estimates(national=TRUE, local=FALSE, mycountry=NULL, main_path = "results/test", pkg_data)
#'
#' Subnational single-country example:
#' get_point_estimates(national=FALSE, local=TRUE, mycountry="Nepal", main_path = "results/test", pkg_data)
#'
#' Subnational multi-country example:
#' get_point_estimates(national=FALSE, local=FALSE, mycountry=NULL, main_path = "results/test", pkg_data)
#'
#' @export

get_point_estimates <- function(national=TRUE, local=FALSE, mycountry=NULL, main_path, pkg_data) {
  if(national==TRUE) {
    get_national_P_point_estimates(main_path, pkg_data, local=local, mycountry=mycountry)
  } else {
    get_subnational_P_point_estimates(main_path, pkg_data, local=local, mycountry=mycountry)
  }
  print("Results complete!")
}
