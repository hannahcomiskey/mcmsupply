#' Connect islands for adjancy matrix GIS
#' Source: https://gis.stackexchange.com/questions/413159/how-to-assign-a-neighbour-status-to-unlinked-polygons
#' @name mstconnect
#' @param polys Spatial polygons to connect
#' @param nb Disjointed neighbourhood matrix
#' @param distance Default is "centroid".
#' @export

mstconnect <- function(polys, nb, distance="centroid"){
  if(distance == "centroid"){
    coords = sf::st_coordinates(sf::st_centroid(sf::st_geometry(polys)))
    dmat = as.matrix(dist(coords))
  }else if(distance == "polygon"){
    dmat = sf::st_distance(polys) + 1 # offset for adjacencies
    diag(dmat) = 0 # no self-intersections
  }else{
    stop("Unknown distance method")
  }

  gfull = igraph::graph.adjacency(dmat, weighted=TRUE, mode="undirected")
  gmst = igraph::mst(gfull)
  edgemat = as.matrix(igraph::as_adj(gmst))
  edgelistw = spdep::mat2listw(edgemat)
  edgenb = edgelistw$neighbour
  attr(edgenb,"region.id") = attr(nb, "region.id")
  allnb = spdep::union.nb(nb, edgenb)
  allnb
}
