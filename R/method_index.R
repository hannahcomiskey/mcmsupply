#' Indexing Functions for methods used in data.
#' @param my_data The data with a  method column you want to index.
#' @param  my_methods A vector of methods used in the data
#' @return returns data with methods indexed
#' @export

method_index_fun <- function(my_data, my_methods) {
  my_data$index_method <- rep(NA, nrow(my_data))
  for (i in 1:length(my_methods)) {
    for (j in 1:nrow(my_data)) {
      method_name <- my_methods[i]
      if(my_data$Method[j]==method_name) {
        my_data$index_method[j] <- i
      }
      else {
        next
      }
    }
  }
  return(my_data)
}
