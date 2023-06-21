#' Wrapper function that retrieves the DHS data used for modelling the proportion of modern contraceptives supplied by the public and private sectors at the national and subnational administration levels.
#' @name get_data
#' @param national TRUE/FALSE. Default is TRUE for national administration level data. FALSE retrieves subnational level data.
#' @param local TRUE/FALSE. Default is FALSE for global runs. Decides if this is a single-country or global run.
#' @param mycountry The name of country of interest. Default is NULL. For the names of potential countries, review vigentte.
#' @param fp2030 Default is TRUE. Filter raw data to only include the Family Planning 2030 focus countries discussed in the Comiskey et al. paper.
#' @param surveydata_filepath Path to survey data. Default is NULL. Survey data should be a .xlsx with the following format \code{\link{national_FPsource_data}}.
#' @return returns a list containing the DHS data set used for inputs into the model and the arguments that specify the data set up.
#' @import R2jags runjags tidyverse tidybayes stats doMC foreach rlang
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' raw_data <- get_data(local=TRUE, mycountry="Nepal")
#' raw_data <- get_data(local=FALSE)
#' raw_data <- get_data(national=FALSE, local=TRUE, mycountry="Nepal")
#' raw_data <- get_data(national=FALSE, local=FALSE)
#' }
#' @export

get_data <- function(national=TRUE, local=FALSE, mycountry=NULL, fp2030=TRUE, surveydata_filepath=NULL) {
  if(national==TRUE) {
    mydata <- get_national_data(local=local, mycountry=mycountry, fp2030=fp2030, surveydata_filepath=surveydata_filepath)
  } else {
    mydata <- get_subnational_data(local=local, mycountry=mycountry, fp2030=fp2030, surveydata_filepath=surveydata_filepath)
  }
  args <- list(national=national, local=local, mycountry=mycountry, fp2030=fp2030) # inherit arguments for next steps
  return(list(mydata = mydata,
              args = args))
}
