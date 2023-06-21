#' Wrapper function to plot the JAGS estimates
#' @name plot_estimates
#' @param jagsdata Output of the `mcmsupply::get_modelinputs()` function.
#' @param ... Arguments from the `mcmsupply::get_modelinputs()` function.
#' @examples
#' \dontrun{
#' raw_data <- get_data(national=FALSE, local=TRUE, mycountry="Nepal")
#' jagsdata <- get_modelinputs(startyear=1990, endyear=2030.5, nsegments=12, raw_data)
#' mod <- run_jags_model(jagsdata)
#' plots <- plot_estimates(jagsdata)
#' }
#' @export

plot_estimates <- function(jagsdata, ...) {
  args <- jagsdata$args # Unpack arguments for specification
  national <- args$national
  if(national==TRUE) {
    p <- plot_national_point_estimates(pkg_data=jagsdata$modelinputs, local=args$local, mycountry=args$mycountry)
  } else {
    p <- plot_subnational_point_estimates(pkg_data=jagsdata$modelinputs, local=args$local, mycountry=args$mycountry)
  }
  message("Plots complete!")
  return(p)
}
