#' Plot median, 95% and 80% credible intervals for posterior samples of P from local JAGS model with the relevant survey data
#' @name plot_subnational_point_estimates
#' @param Psamps Posterior samples of P for one sector from JAGS model
#' @param pkg_data Output of the `mcmsupplylocal::get_subnational_modelinputs()` function.
#' @param vis_path String. Path where your visualisations will be saved to.
#' @return Data frame of labelled posterior samples with median, 95% and 80% credible intervals estimates.
#' @export

plot_subnational_point_estimates <- function(main_path, pkg_data, vis_path, local=FALSE, mycountry=NULL) {

  if(local==TRUE & is.null(mycountry)==FALSE) {
    P_samps <- readRDS(paste0(main_path, mycountry,"_P_point_estimates.RDS"))
  } else{
    P_samps <- readRDS(paste0(main_path,"P_point_estimates.RDS"))
  }

  # Get model inputs for cross matching to estimates
  n_country <- pkg_data$n_country
  n_subnat <- pkg_data$n_subnat
  n_method <- pkg_data$n_method
  n_sector <- c("Public", "Commercial_medical", "Other")
  n_all_years <- pkg_data$n_years
  mydata <- pkg_data$data
  all_years <- pkg_data$all_years

  # Creating index tables for reference
  subnat_index_table <- mydata %>% select(Country, Region, index_subnat) %>% ungroup() %>% distinct()
  country_index_table <- tibble(Country = n_country, index_country = unique(mydata$index_country))
  method_index_table <- tibble(Method = n_method, index_method = 1:length(n_method))
  sector_index_table <- tibble(Sector = n_sector, index_sector = 1:length(n_sector))
  year_index_table <- tibble(average_year = all_years,
                             index_year = 1:n_all_years,
                             floored_year = floor(all_years))

  # Recreating data used in model for plotting
  FP_source_data_long_SE <- mydata %>%
    dplyr::select(Country, Region, Method, average_year, Commercial_medical.SE, Public.SE, Other.SE) %>%
    dplyr::rename(Commercial_medical = Commercial_medical.SE , Public = Public.SE, Other = Other.SE) %>%
    tidyr::gather(Sector, SE.proportion, Commercial_medical:Other, factor_key=TRUE)

  FP_source_data_long <- mydata %>%
    dplyr::select(Country, Region, Method, average_year, Commercial_medical, Public, Other) %>%
    tidyr::gather(Sector, proportion, Commercial_medical:Other, factor_key=TRUE)

  FP_source_data_long <- merge(FP_source_data_long, FP_source_data_long_SE)

  # Adding error limits to data using SE
  FP_source_data_long <- FP_source_data_long %>%
    dplyr::mutate( prop_min = proportion - 2*SE.proportion,
            prop_max = proportion + 2*SE.proportion ) %>%
    dplyr::mutate(prop_max = ifelse(prop_max > 1, 1, prop_max)) %>%
    dplyr::mutate(prop_min = ifelse(prop_min < 0, 0, prop_min))

  # Country estimates
  safe_colorblind_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  for(i in 1:nrow(subnat_index_table)) {
    country_data <- FP_source_data_long %>%
      dplyr::filter(Country==subnat_index_table$Country[i] & Region==subnat_index_table$Region[i]) %>%
      dplyr::rename(Sector = Sector)
    country_calc <- P_samps %>%
      dplyr::filter(Country==subnat_index_table$Country[i] & Region==subnat_index_table$Region[i])

    ci_plot = ggplot2::ggplot() +  # plot of true p value vs time using facet wrap
      ggplot2::geom_line(data=country_calc, ggplot2::aes(x=average_year, y=median_p, color=Sector)) +
      ggplot2::geom_point(data=country_data, ggplot2::aes(x=average_year, y=proportion, colour=Sector))+
      ggplot2::geom_errorbar(data=country_data, ggplot2::aes(ymin = prop_min, ymax = prop_max, x=average_year, colour=Sector), width = 1.5) +
      ggplot2::geom_ribbon(data=country_calc, ggplot2::aes(ymin = lower_95, ymax = upper_95, x=average_year, fill=Sector), alpha=0.2) +
      ggplot2::geom_ribbon(data=country_calc, ggplot2::aes(ymin = lower_80, ymax = upper_80, x=average_year, fill=Sector), alpha=0.26) +
      ggplot2::labs(y="Proportion of contraceptives supplied", x = "Year", title = paste0(unique(country_calc$Region),", ",unique(country_data$Country))) +
      ggplot2::scale_y_continuous(limits=c(0,1))+
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), strip.text.x = ggplot2::element_text(size = 9)) +
      ggplot2::theme(legend.position = "bottom")+
      ggplot2::labs(fill = "Sector") +
      ggplot2::guides(color="none") +
      ggplot2::facet_wrap(~Method)

    country_name <- subnat_index_table$Country[i]
    region_name <- stringr::str_replace_all(subnat_index_table$Region[i], "[[:punct:]]", "_") # remove special characters
    region_name <- stringr::str_replace_all(region_name, " ", "") # remove spaces

    ggplot2::ggsave(ci_plot, filename = paste0(country_name,"_",region_name,"_p.pdf"), path = vis_path, height=12, width=15)
  }
}
