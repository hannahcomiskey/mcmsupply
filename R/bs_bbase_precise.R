#' Get precisely aligned basis functions
#' @name bs_bbase_precise
#' @param x The vector of years you wish to create your basis functions over
#' @param lastobs The year of the most recent survey you wish to align the knots with. Default is max(x).
#' @param xl Default is xl = min(x)
#' @param xr Default is xr = max(x)
#' @param nseg Number of knots you wish to use
#' @param deg The degree of the polynomial. Default is 3.
#'
#' @return B.ik is a matrix, each row is one observation, each column is one B-spline.
#' knots.k is a vector of transformed knots.
#' Kstar is the knot point of last observation
#' @export
#'
#' @examples all_years <- seq(from = 1990, to = 2030.5, by=0.5)
#' bs_bbase_precise(all_years, lastobs=2014.5, nseg = 12)

bs_bbase_precise <- function(x = x,lastobs = max(x), xl = min(x), xr = max(x), nseg = nseg, deg = 3) {
  # Compute the length of the partitions
  dx <- (xr - xl) / nseg
  # Compute position of knot before last observation
  dk <- lastobs
  # Create equally spaced knots
  knots <- seq(xl - deg * dx, xr + deg * dx, by = dx)
  # Find index of closest knot to dk
  dk_index <- which.min(abs(knots-dk))
  # Find transformation to knot placement so that dk is a knot
  ktrans <- (dk-knots)[dk_index]
  # Add transformation to knots
  knotsnew <- knots + ktrans
  # Use bs() function to generate the B-spline basis
  get_bs_matrix <- matrix(splines::bs(x, knots = knotsnew, degree = deg, Boundary.knots = c(knotsnew[1], knotsnew[length(knotsnew)])), nrow = length(x))

  # Remove columns that contain zero only
  bs_matrix <- get_bs_matrix[, -c(1:deg, ncol(get_bs_matrix):(ncol(get_bs_matrix) - deg))]

  used_knots <- knotsnew[-c(1,2,length(knotsnew),(length(knotsnew)-1))]
  Kstar <- which(used_knots==dk)

  return(list(B.ik = bs_matrix, ##<< Matrix, each row is one observation, each column is one B-spline.
              knots.k = used_knots, ##<< Vector of transformed knots.
              Kstar = Kstar # Knot point of last observation
  ))
}
