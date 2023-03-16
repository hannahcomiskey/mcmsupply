#' Standardise the contraceptive method names
#'
#' @param my_data The input data with a Method column to standardise
#'
#' @return Original input dataframe with Method column standardised.
#' @export
#'
#' @examples data_standard <- standard_method_names(mydata)
standard_method_names <- function(my_data) {
  my_data <- my_data %>%
    dplyr::mutate(Method = replace(Method, Method == "Condom (m+f)", "Condom")) %>%
    dplyr::mutate(Method = replace(Method, Method == "male condom", "Condom")) %>%
    dplyr::mutate(Method = replace(Method, Method == "condom", "Condom")) %>%
    dplyr::mutate(Method = replace(Method, Method == "Sterilization (female)", "Female Sterilization")) %>%
    dplyr::mutate(Method = replace(Method, Method == "female sterilization", "Female Sterilization")) %>%
    dplyr::mutate(Method = replace(Method, Method == "Female sterilization", "Female Sterilization")) %>%
    dplyr::mutate(Method = replace(Method, Method == "Sterilization (male)", "Male Sterilization")) %>%
    dplyr::mutate(Method = replace(Method, Method == "Male sterilization", "Male Sterilization")) %>%
    dplyr::mutate(Method = replace(Method, Method == "male sterilization", "Male Sterilization")) %>%
    dplyr::mutate(Method = replace(Method, Method == "Pill", "OC Pills")) %>%
    dplyr::mutate(Method = replace(Method, Method == "pill", "OC Pills")) %>%
    dplyr::mutate(Method = replace(Method, Method == "injections", "Injectables")) %>%
    dplyr::mutate(Method = replace(Method, Method == "implants", "Implants")) %>%
    dplyr::mutate(Method = replace(Method, Method %in% c("Other", "Other Modern Methods", "LAM", "diaphragm", "female condom", "foam or jelly", "standard days method", "diaphragm, foam or jelly", "lactational amenorrhea", "emergency contraception", "other modern methods"), "Other Modern Methods"))

  return(my_data)
}
