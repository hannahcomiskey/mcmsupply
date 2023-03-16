#' Flatten correlation matrix
#' @name flat_cor_mat
#' @param cor_r Correlation matrix
#' @return A table with 3 columns containing :
# Column 1 : row names (variable 1 for the correlation test)
# Column 2 : column names (variable 2 for the correlation test)
# Column 3 : the correlation coefficients
#' @export
#'
#' @examples R <- Hmisc::rcorr(as.matrix(mtcars[,1:7])) # sample correlation matrix
#' flat_corr <- flat_cor_mat(R)
flat_cor_mat <- function(cor_r){
  cor_r <- tibble::rownames_to_column(as.data.frame(cor_r), var = "row")
  cor_r <- dplyr::gather(cor_r, column, cor, -1)
  cor_r <- cor_r %>% dplyr::distinct(cor, .keep_all = TRUE)
  cor_r$cor <- round(cor_r$cor,1)
  cor_r <- cor_r %>% dplyr::filter(cor!=1)
  return(cor_r)
}
