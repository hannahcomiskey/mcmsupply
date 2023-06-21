#' The median estimate for the subnational-level variance-covariance matrix of the delta.k terms in the multi-country subnational model.
#' This array is used to inform the Wishart prior of the single-country subnational model.
#'
#' @docType data
#' @keywords sigma_delta_hat
#' @format  A array of 2 matrices with 5 rows and 5 columns:
#' \describe{
#'   \item{Array 1}{Estimated public sector variance-covaraince matrix}
#'   \item{Array 2}{Estimated private sector variance-covaraince matrix}
#'   }
"sigma_delta_hat"
