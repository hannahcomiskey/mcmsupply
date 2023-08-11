#' The median estimate for the subnational-level precision matrix of the delta.k terms in the multi-country subnational model.
#' This array is used to inform the multi-variate normal prior in the single-country subnational model.
#'
#' @docType data
#' @keywords subnational_inv.sigma_delta_hat
#' @format  A array of 2 matrices with 5 rows and 5 columns:
#' \describe{
#'   \item{Array 1}{Estimated public sector precision matrix}
#'   \item{Array 2}{Estimated private sector precision matrix}
#'   }
"subnational_inv.sigma_delta_hat"
