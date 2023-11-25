#' Function to pull method-supply share median estimates and credible intervals for a given year and country.
#' @name pull_estimates
#' @param model_output The output of the mcmsupply::run_jags_model() function.
#' @param year Numeric. The year of model estimated you wish to pull.
#' @param country String. The name of the country you wish to inspect.
#' @examples
#' \dontrun{
#   raw_data <- get_data(national=TRUE, local=TRUE, mycountry="Nepal")
#   jagsdata <- get_modelinputs(startyear=1990, endyear=2020.5, nsegments=12, raw_data)
#   mod <- run_jags_model(jagsdata = jagsdata, jagsparams = NULL, n_iter = 5, n_burnin = 1, n_thin = 1)
#   estimates <- pull_estimates(model_output = mod, year=2018, country="Nepal")
#' }
#' @return A list of model estimates for each method.
#' @export

pull_estimates <- function(model_output, year, country) {
  year_av <- floor(as.numeric(year))+0.5 # floor year and add 0.5 to match mid-year estimates

  # Get models estimates
  estimates <- model_output$estimates

  # Country estimates
  p <- estimates[which(estimates$Country==country & estimates$average_year==year_av), ] #%>% filter(sector_category=="Public")

  message("Estimates ready!")
  return(p)
}
