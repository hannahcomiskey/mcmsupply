#' Apply standard naming for modern contraceptive methods to dataset
#' @name standard_method_names
#' @param my_data A dataframe or tibble containing a column called "Method" which contains the names of modern contraceptive methods
#' @return A dataframe or tibble that has standardised naming applied to the Method column
#' @export
standard_method_names <- function(my_data) {
  levels(my_data$Method) <- c(levels(my_data$Method), "Condom", "Female Sterilization", "Male Sterilization", "Other Modern Methods", "OC Pills")

  my_data <- my_data %>%
    dplyr::mutate(Method = replace(Method, Method == "Condom (m+f)", "Condom")) %>%
    dplyr::mutate(Method = replace(Method, Method == "male condom", "Condom")) %>%
    dplyr::mutate(Method = replace(Method, Method == "condom", "Condom")) %>%
    dplyr::mutate(Method = replace(Method, Method == "female condom", "Condom")) %>%
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
    dplyr::mutate(Method = replace(Method, Method %in% c("Other", "Other Modern Methods", "LAM", "diaphragm", "foam or jelly", "standard days method", "diaphragm, foam or jelly", "lactational amenorrhea", "emergency contraception", "other modern methods"), "Other Modern Methods")) %>%
    as.data.frame()

  return(my_data)
}
