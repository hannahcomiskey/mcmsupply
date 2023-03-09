#' DHS survey observations for the proportion of modern contraceptives supplied by the public and private sectors at the national level
#'
#' @format ## `national_FPsource_data`
#' A data frame with 2459 rows and 11 columns:
#' \describe{
#'   \item{year}{Year of survey}
#'   \item{Country}{Country names}
#'   \item{Method}{Contraceptive method name}
#'   \item{average_year}{Average year of the survey}
#'   \item{sector_category}{Name of sector}
#'   \item{proportion}{Proportion supplied by the sector}
#'   \item{SE.proportion}{Standard error associated with the proportion}
#'   \item{country_code}{Letter code for country name}
#'   \item{n}{Sample size associated with the observation}
#'   \item{check_sum}{Evaluating the total of the proportions across the three sectors for a given year, method and country}
#'   \item{Method_collapse}{Standardised method names}
#' }
#' @source On request from DHS microdata - using the womens individuals recode file. Contact details found at https://dhsprogram.com/data/dataset_admin/login_main.cfm
"national_FPsource_data"
