#' Get B-splines. Written by Dr. Niamh Cahill.
#' @name bs_bbase_precise
#' @param x  Vector of x-values (without NAs) for which splines need to be calculated (determines the number of rows of B.ik)
#' @param lastobs = max(x) max x-value which determines knot placement. Aligns knot to most recent survey year.
#' @param xl = min(x) min x-value which determines knot placement.
#' @param xr = max(x) max x-value which determines knot placement
#' @param nseg Number of segments to divide x vector into, used to place knots
#' @param degree = 3 # currently tested only with degree 3
#' @return returns B.ik matrix, each row is one observation, each column is one B-spline. Vector of knots indicating knot placement across x vector.
#' @export


bs_bbase_precise <- function(x = x,lastobs = max(x), xl = min(x), xr = max(x), nseg = 10, deg = 3) {
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
