#' Apply superregion indexing to data
#'
#' @param my_data The sub-national family planning source data for the country of interest.
#' @param n_region A vector of the super-regions listed in the dataset
#'
#' @return Dataframe with super-region indexing applied
#' @export
#'
#' @examples n_superreg <- unique(mydata$Super_region)
#' mydata <- superregion_index_fun(mydata, n_superreg)
superregion_index_fun <- function(my_data, n_region) {
  my_data$index_superregion <- NA
  for (i in 1:length(n_region)) {
    for (j in 1:nrow(my_data)) {
      region_name <- n_region[i]
      if(my_data$Super_region[j]==region_name) {
        my_data$index_superregion[j] <- i
      }
      else {
        next
      }
    }
  }
  return(my_data)
}
