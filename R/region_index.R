#' Indexing function for regions used in data.
#' @param my_data The data with a Region column you want to index.
#' @param  n_region A vector of regions used in the data
#' @return returns data with region indexed
#' @export

region_index_fun <- function(my_data, n_region) {
  my_data$index_region <- NA
  for (i in 1:length(n_region)) {
    for (j in 1:nrow(my_data)) {
      region_name <- n_region[i]
      if(my_data$Region[j]==region_name) {
        my_data$index_region[j] <- i
      }
      else {
        next
      }
    }
  }
  return(my_data)
}

