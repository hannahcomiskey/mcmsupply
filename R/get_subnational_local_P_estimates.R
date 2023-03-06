#' Get median, 95% and 80% credible intervals for posterior samples of P from local JAGS model
#' @name get_subnational_local_P_estimates
#' @param Psamps posterior samples of P for one sector from JAGS model
#' @param param_names names of the parameters you wish to summarise
#' @param subnat_index_table Dataframe with subnational district indexing applied. Used to match estimates to data.
#' @param method_index_table Dataframe with method indexing applied. Used to match estimates to data.
#' @param sector_type String. Name of sector you are interested in. One of either ("Public", "Commercial medical", "Other")
#' @param year_index_table Dataframe with time indexing applied. Used to match estimates to data.
#' @return Dataframe of labelled posterior samples with median, 95% and 80% credible intervals estimates.
#' @export
#'
get_subnational_local_P_estimates <- function(Psamps, param_names, subnat_index_table, method_index_table, sector_index_table, year_index_table) {
  samp_med <- Psamps %>%
    dplyr::summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE))) %>%
    dplyr::select(all_of(param_names)) %>%
    tidyr::pivot_longer(all_of(param_names),
                        names_to = "params",
                        values_to = "median_p") %>%
    tidyr::separate(params, c("P" ,"index_sector", "index_method", "index_subnat", "index_year")) %>%
    dplyr::select(!P)

  samp_lwr95 <- Psamps %>%
    dplyr::summarise(across(where(is.numeric), ~ quantile(.x, prob = 0.025, na.rm = TRUE))) %>%
    dplyr::select(all_of(param_names)) %>%
    tidyr::pivot_longer(all_of(param_names),
                        names_to = "params",
                        values_to = "lower_95") %>%
    tidyr::separate(params, c("P" ,"index_sector", "index_method", "index_subnat", "index_year")) %>%
    dplyr::select(!P)

  samp_upr95 <- Psamps %>%
    dplyr::summarise(across(where(is.numeric), ~ quantile(.x, prob = 0.975, na.rm = TRUE))) %>%
    dplyr::select(all_of(param_names)) %>%
    tidyr::pivot_longer(all_of(param_names),
                        names_to = "params",
                        values_to = "upper_95") %>%
    tidyr::separate(params, c("P" ,"index_sector", "index_method", "index_subnat", "index_year")) %>%
    dplyr::select(!P)

  samp_lwr80 <- Psamps %>%
    dplyr::summarise(across(where(is.numeric), ~ quantile(.x, prob = 0.1, na.rm = TRUE))) %>%
    dplyr::select(all_of(param_names)) %>%
    tidyr::pivot_longer(all_of(param_names),
                        names_to = "params",
                        values_to = "lower_80") %>%
    tidyr::separate(params, c("P" ,"index_sector", "index_method", "index_subnat", "index_year")) %>%
    dplyr::select(!P)

  samp_upr80 <- Psamps %>%
    dplyr::summarise(across(where(is.numeric), ~ quantile(.x, prob = 0.9, na.rm = TRUE))) %>%
    dplyr::select(all_of(param_names)) %>%
    tidyr::pivot_longer(all_of(param_names),
                        names_to = "params",
                        values_to = "upper_80") %>%
    tidyr::separate(params, c("P" ,"index_sector", "index_method", "index_subnat", "index_year")) %>%
    dplyr::select(!P)

  tmp_summary <- samp_med %>%
    dplyr::left_join(samp_lwr95) %>%
    dplyr::left_join(samp_upr95) %>%
    dplyr::left_join(samp_lwr80) %>%
    dplyr::left_join(samp_upr80) %>%
    dplyr::mutate(across(index_sector:upper_80, as.numeric)) %>%
    dplyr::left_join(year_index_table) %>%
    dplyr::left_join(method_index_table) %>%
    dplyr::left_join(sector_index_table) %>%
    dplyr::left_join(subnat_index_table) %>%
    dplyr::group_by(Region, Method, Sector, average_year) %>%
    dplyr::select(Country, Region, Method, Sector, average_year, median_p, lower_95, upper_95, lower_80, upper_80) %>%
    dplyr::distinct() %>%
    dplyr::rowwise() %>%
    dplyr::filter(average_year > floor(average_year))

  return(tmp_summary)
}
