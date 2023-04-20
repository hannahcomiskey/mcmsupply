#' Wrapper function to plot the JAGS estimates
#' @name plot_estimates
#' @param main_path String. Default is "results/". Path where you have set your model results to be saved to.
#' @param vis_path String. Default is "visualisation/". Path where your visualisations will be saved to.
#' @param jagsdata Output of the `mcmsupply::get_modelinputs()` function.
#' @param ... Arguments from the `mcmsupply::get_modelinputs()` function.
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

plot_estimates <- function(main_path=NULL, vis_path=NULL, jagsdata, ...) {
  if(is.null(main_path)==TRUE) {
    main_path = "results/" # set default location for results
  }
  if(is.null(vis_path)==TRUE) {
    vis_path = "visualisation/" # set default location for plots
  }
  args <- jagsdata$args # Unpack arguments for specification
  national <- args$national
  if(national==TRUE) {
    plot_national_point_estimates(main_path=main_path, vis_path=vis_path, pkg_data=jagsdata$modelinputs, local=args$local, mycountry=args$mycountry)
  } else {
    plot_subnational_point_estimates(main_path=main_path, vis_path=vis_path, pkg_data=jagsdata$modelinputs, local=args$local, mycountry=args$mycountry)
  }
  print("Plots complete!")
}
