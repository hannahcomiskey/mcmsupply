#' Wrapper function to plot the JAGS estimates
#' @name plot_estimates
#' @param jagsdata Output of the mcmsupply::get_modelinputs() function.
#' @param model_output The output of the mcmsupply::run_jags_model() function.
#' @examples
#' \donttest{
#' raw_data <- get_data(national=FALSE, local=TRUE, mycountry="Nepal")
#' jagsdata <- get_modelinputs(startyear=1990, endyear=2030.5, nsegments=12, raw_data)
#' mod <- run_jags_model(jagsdata)
#' plots <- plot_estimates(jagsdata)
#' }
#' @return A list of ggplot objects.
#' @export

plot_estimates <- function(jagsdata, model_output) {
  args <- jagsdata$args # Unpack arguments for specification
  national <- args$national
  if(national==TRUE) {
    p <- plot_national_point_estimates(pkg_data=jagsdata$modelinputs, model_output = model_output, local=args$local, mycountry=args$mycountry)
  } else {
    p <- plot_subnational_point_estimates(pkg_data=jagsdata$modelinputs, model_output = model_output, local=args$local, mycountry=args$mycountry)
  }
  message("Plots complete!")
  return(p)
}
