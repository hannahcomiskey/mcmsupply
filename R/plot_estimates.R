#' Wrapper function to plot the JAGS estimates
#' @name plot_estimates
#' @param national TRUE/FALSE. Default is TRUE for national administration level data. FALSE retrieves subnational level data.
#' @param main_path String. Path where you have set your model results to be saved to.
#' @param vis_path String. Path where your visualisations will be saved to.
#' @param jagsdata Output of the `mcmsupply::get_modelinputs()` function.
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @examples National single-country example:
#' plot_estimates(main_path = "results/test", vis_path  = "visualisations/test", jagsdata, ...)
#'
#' National multi-country example:
#' plot_estimates(main_path = "results/test", vis_path  = "visualisations/test", jagsdata, ...)
#'
#' Subnational single-country example:
#' plot_estimates(main_path = "results/test", vis_path  = "visualisations/test", jagsdata, ...)
#'
#' Subnational multi-country example:
#' plot_estimates(main_path = "results/test", vis_path  = "visualisations/test", jagsdata, ...)
#'
#' @export

plot_estimates <- function(main_path, vis_path, jagsdata, ...) {
  args <- jagsdata$args
  national <- args$national
  if(national==TRUE) {
    plot_national_point_estimates(main_path=main_path, vis_path=vis_path, pkg_data=jagsdata$modelinputs, local=args$local, mycountry=args$mycountry)
  } else {
    plot_subnational_point_estimates(main_path=main_path, vis_path=vis_path, pkg_data=jagsdata$modelinputs, local=args$local, mycountry=args$mycountry)
  }
  print("Plots complete!")
}
