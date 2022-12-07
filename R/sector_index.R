#' Indexing function for sectors used in data.
#' @param my_data The data with a sector_category column you want to index.
#' @param  my_sectors A vector of sectors used in the data
#' @return returns data with sectors indexed
#' @export

sector_index_fun <- function(my_data, my_sectors) {
  for (i in 1:length(my_sectors)) {
    for (j in 1:nrow(my_data)) {
      sector_name <- my_sectors[i]
      if(my_data$sector_category[j]==sector_name) {
        my_data$index_sector[j] <- i
      }
      else {
        next
      }
    }
  }
  return(my_data)
}
