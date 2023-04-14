#' Get JAGS model inputs
#' @name get_subnational_modelinputs
#' @param national TRUE/FALSE. Default is TRUE for national administration level data. FALSE retrieves subnational level data.
#' @param fp2030 TRUE/FALSE. Default is TRUE. Filters the data to only include FP2030 countries.
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @param startyear The year you wish to begin your predictions from. Default is 1990.
#' @param endyear The year you wish to finish your predictions. Default is 2030.5.
#' @param nsegments The number of knots you wish to use in your basis functions. Default is 12.
#' @param raw_data The subnational family planning source data from the 'get_subnational_data' function.
#'
#' @return A list of modelling inputs for the JAGS model.
#' 1. Tstar is the year index for the most recent survey in each province.
#' 2. Kstar is the knot index that aligns with Tstar.
#' 3. B.ik are the basis functions.
#' 4. n_years are total number of years
#' 5. n_obs  are the total number of observations
#' 6. K are the number of knots.
#' 7. H is K-1. Used in the calculation of first order differences of spline coefficients.
#' 8. P_count is the number of subnational provinces/regions.
#' 9. M_count is the number of modern contraceptive methods.
#' 10. matchsubnat is the subnational province indexing to match the observed data to the predictions.
#' 11. matchcountry is the country indexing to match the observed data to the predictions.
#' 12. matchmethod is the method indexing to match the observed data to the predictions.
#' 13. matchyears is the year indexing to match the observed data to the predictions.
#' @examples National single-country example:
#' jagsdata <- get_modelinputs(national=TRUE, fp2030=TRUE, local=TRUE, mycountry="Nepal", startyear=1990, endyear=2030.5, nsegments=12, raw_data)
#'
#' National multi-country example:
#' jagsdata <- get_modelinputs(national=TRUE, fp2030=TRUE, local=FALSE, mycountry=NULL, startyear=1990, endyear=2030.5, nsegments=12, raw_data)
#'
#' Subnational single-country example:
#' jagsdata <- get_modelinputs(national=FALSE, fp2030=TRUE, local=TRUE, mycountry="Nepal", startyear=1990, endyear=2030.5, nsegments=12, raw_data)
#'
#' Subnational multi-country example:
#' jagsdata <- get_modelinputs(national=FALSE, fp2030=TRUE, local=FALSE, mycountry=NULL, startyear=1990, endyear=2030.5, nsegments=12, raw_data)
#' @export

get_modelinputs <- function(national=TRUE, fp2030=TRUE, local=FALSE, mycountry=NULL, startyear=1990, endyear=2030.5, nsegments=12, raw_data) {
  if(national==TRUE) {
    modelinputs <- get_national_modelinputs(fp2030=fp2030, local=local, mycountry=mycountry, startyear=startyear, endyear=endyear, nsegments=nsegments, raw_data)
  } else {
    modelinputs <- get_subnational_modelinputs(fp2030=fp2030, local=local, mycountry=mycountry, startyear=startyear, endyear=endyear, nsegments=nsegments, raw_data)
  }
  return(modelinputs)
}
