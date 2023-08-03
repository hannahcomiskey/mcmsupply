#' The Country and area classification according to the United Nations Standaistical Division, Standard country or area codes for statistical use (M49).
#' Adapted for use in FP2030 by the Track20 project.
#' A subset of data from the United Nations country classifications
#'
#' @docType data
#' @keywords Country_and_area_classification
#' @format  A data frame with 231 rows and 8 columns:
#' \describe{
#'   \item{Country or area}{Country name}
#'   \item{ISO Code}{1, 2 & 3 number ISO country codes}
#'   \item{Major area}{Continent}
#'   \item{Region}{Sub-continent}
#'   \item{Developed region}{Binary indicator for development status}
#'   \item{Least developed country}{Binary indicator for least developed status}
#'   \item{Sub-Saharan Africa}{Binary indicator for whether country is in Sub-Saharan Africa}
#'   \item{FP2020}{Binary indicator for FP2020 participation status}
#' }
#' @source https://unstats.un.org/unsd/methodology/m49/
"Country_and_area_classification"
