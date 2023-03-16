#' Apply method indexing to data
#'
#' @param my_data The sub-national family planning source data for the country of interest.
#' @param my_methods A vector of your contraceptive methods in the order you wished them indexed.
#'
#' @return Dataframe with method indexing applied
#' @export
#'
#' @examples   n_method <- c("Female Sterilization", "Implants", "Injectables", "IUD", "OC Pills" ) # As per the method correlation matrix
#' method_index_fun(mydata, n_method)

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
