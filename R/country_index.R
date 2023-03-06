#' Indexing function for countries used in data.
#' @describeIn country_index_fun Calculates the country index for named countries
#' @param my_data The data with a Country column you want to index.
#' @param  my_countries A vector of country names used in the data
#' @return returns table with countries indexed
#' @export

country_index_fun <- function(my_data, my_countries) {
  for (i in 1:length(my_countries)) {
    for (j in 1:nrow(my_data)) {
      country_name <- my_countries[i]
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


#' @describeIn country_index_fun Calculates the continent index for named continents
continent_index_fun <- function(my_data) {
  n_con <- as.character(unique(my_data$continent))
  my_data$index_continent <- NA
  for (i in 1:length(n_con)) {
    for (j in 1:nrow(my_data)) {
      con_name <- n_con[i]
      if (my_data$continent[j] == con_name) {
        my_data$index_continent[j] <- i
      } else {
        next
      }
    }
  }
  return(my_data)
}
