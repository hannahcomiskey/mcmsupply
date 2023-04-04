#' Get neighbourhood adjacency matrix for your country of interest. Calculate your neighbour count.
#' @name get_subnational_local_geodata
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @return A list of two matrices. D = Neighbour count matrix, neighbours = Neighbourhood adjacency matrix.
#' @export
#' @examples
#' get_subnational_local_geodata("Nepal")

get_subnational_local_geodata <- function(mycountry) {

  tmp_nb1 <- get(paste0(mycountry, "_neighbouradj")) # Read in local neighbour adjacency matrices
  D <- diag(rowSums(tmp_nb1))

  return(list(D = D,
              W = tmp_nb1))
}
