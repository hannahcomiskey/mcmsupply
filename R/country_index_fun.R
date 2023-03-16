#' Apply country indexing to data
#'
#' @param my_data The sub-national family planning source data for the country of interest.
#' @param my_country The name of your country of interest.
#'
#' @return Dataframe with country indexing applied
#' @export
#'
#' @examples country_index_fun(mydata, "Nepal")

country_index_fun <- function(my_data, my_country) {
  for (i in 1:length(my_country)) {
    for (j in 1:nrow(my_data)) {
      country_name <- my_country[i]
      if(my_data$Country[j]==country_name) {
        my_data$index_country[j] <- i
      }
      else {
        next
      }
    }
  }
  return(my_data)
}
