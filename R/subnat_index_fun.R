#' Apply subnational indexing to data
#'
#' @param my_data The sub-national family planning source data for the country of interest.
#' @param my_subnat A vector of the provinces/region names in your country of interest.
#' @param my_country The name of your country of interest. Must be a vector the same length as my_subnat.
#'
#' @return Dataframe with subnational indexing applied
#' @export
#'
#' @examples country_subnat_tbl <- mydata %>%
#' dplyr::group_by(Country, Region) %>%
#' dplyr::select(Country, Region) %>%
#' dplyr::distinct() # table of country and regions (removes repeats in region names)
#'
#' subnat_index_fun(mydata, country_subnat_tbl$Region, country_subnat_tbl$Country)
subnat_index_fun <- function(my_data, my_subnat, my_country) {
  for (i in 1:length(my_subnat)) {
    for (j in 1:nrow(my_data)) {
      subnat_name <- my_subnat[i]
      country_name <- my_country[i]
      if(my_data$Country[j]==country_name & my_data$Region[j]==subnat_name) {
        my_data$index_subnat[j] <- i
      }
      else {
        next
      }
    }
  }
  return(my_data)
}
