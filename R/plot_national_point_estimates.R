#' Plot median and 95% credible intervals for posterior samples of P from JAGS model with the relevant survey data
#' @name plot_national_point_estimates
#' @param main_path String. Path where you have set your model results to be saved to.
#' @param vis_path String. Path where your visualisations will be saved to.
#' @param pkg_data Output of the `mcmsupplylocal::get_subnational_modelinputs()` function.
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @return Data frame of labelled posterior samples with median, 95% and 80% credible intervals estimates.
#' @export

plot_national_point_estimates <- function(main_path, vis_path, pkg_data, local=FALSE, mycountry=NULL) {

  if(local==TRUE & is.null(mycountry)==FALSE) {
    P_samps <- readRDS(paste0(main_path,mycountry,"_P_point_national_estimates.RDS"))
  } else{
    P_samps <- readRDS(paste0(main_path,"P_point_national_estimates.RDS"))
  }

  # Get model inputs for cross matching to estimates
  n_country <- pkg_data$n_country
  n_method <- pkg_data$n_method
  n_sector <- c("Public", "Commercial_medical", "Other")
  n_all_years <- pkg_data$n_years
  mydata <- pkg_data$data
  all_years <- pkg_data$all_years

  # Creating index tables for reference
  country_index_table <- tibble(Country = n_country, index_country = unique(mydata$index_country))
  method_index_table <- tibble(Method = n_method, index_method = 1:length(n_method))
  sector_index_table <- tibble(Sector = n_sector, index_sector = 1:length(n_sector))
  year_index_table <- tibble(average_year = all_years,
                             index_year = 1:n_all_years,
                             floored_year = floor(all_years))

  # Recreating data used in model for plotting
  FP_source_data_long_SE <- mydata %>%
    dplyr::select(Country, Method, average_year, Commercial_medical.SE, Public.SE, Other.SE) %>%
    dplyr::rename(Commercial_medical = Commercial_medical.SE , Public = Public.SE, Other = Other.SE) %>%
    tidyr::gather(Sector, SE.proportion, Commercial_medical:Other, factor_key=TRUE)

  FP_source_data_long <- mydata %>%
    dplyr::select(Country, Method, average_year, Commercial_medical, Public, Other) %>%
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
  for(i in n_country) {
    print(i)
    country_data <- FP_source_data_long[which(FP_source_data_long$Country==i), ] #%>% filter(sector_category=="Public")
    country_calc <- P_samps[which(P_samps$Country==i), ] #%>% filter(sector_category=="Public")

    ci_plot = ggplot2::ggplot() +  # plot of true p value vs time using facet wrap
      ggplot2::geom_line(data=country_calc, ggplot2::aes(x=average_year, y=median_p, color=Sector)) +
      ggplot2::geom_point(data=country_data, ggplot2::aes(x=average_year, y=proportion, colour=Sector))+
      ggplot2::geom_errorbar(data=country_data, ggplot2::aes(ymin = prop_min, ymax = prop_max, x=average_year, colour=Sector), width = 1.5) +
      ggplot2::geom_ribbon(data=country_calc, ggplot2::aes(ymin = lower_95, ymax = upper_95, x=average_year, fill=Sector), alpha=0.2) +
      ggplot2::labs(y="Proportion of contraceptives supplied", x = "Year", title = i) +
      ggplot2::scale_y_continuous(limits=c(0,1))+
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), strip.text.x = ggplot2::element_text(size = 9)) +
      ggplot2::theme(legend.position = "bottom")+
      ggplot2::labs(fill = "Sector") +
      ggplot2::guides(color="none") +
      ggplot2::scale_colour_manual(values=safe_colorblind_palette) +
      ggplot2::scale_fill_manual(values=safe_colorblind_palette) +
      ggplot2::facet_wrap(~Method)

    country_name <- i

    ggplot2::ggsave(ci_plot, filename = paste0(country_name,"_p.pdf"), path = vis_path, height=12, width=15)
  }
}
