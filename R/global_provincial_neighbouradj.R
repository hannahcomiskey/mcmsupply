#' Global subnational provincial neighbourhood matrix
#' Neighbourhood matrix for all countries and provinces in the FP2030 subnational dataset.
#' 1 indicates that the provinces share a border. 0 indicates no shared borders.
#' Some countries, eg: Madagascar, are islands. In this case, they are shown to share a border with their closest neighbour.
#' @format ## `global_provincial_neighbouradj`
#' A data frame with 209 rows and 209 columns:
#' \describe{
#'   \item{Afghanistan_Badakhshan}{Binary neighbourhood status for Badakhshan region of Afghanistan with all other provinces, countries globally}
#'   ...
#'   \item{Zimbabwe_Midlands}{Binary neighbourhood status for Midlands region of Zimbabwe with all other provinces, countries globally}
#' }
"global_provincial_neighbouradj"
