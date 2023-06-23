library(hexSticker)
devtools::load_all()
library(showtext)
library(ggimage)

cleaned_natdata <- get_data(national=TRUE, local=TRUE, mycountry="Nepal")
pkg_data <- get_modelinputs(startyear=1990, endyear=2025.5, nsegments=12,
                            raw_data = cleaned_natdata)
mod <- run_jags_model(jagsdata = pkg_data, jagsparams = NULL,
                      n_iter = 50000, n_burnin = 10000, n_thin = 20)

# Get models estimates
estimates <- mod$estimates
pkg_data=pkg_data$modelinputs

# Get model inputs for cross matching to estimates
n_country <- pkg_data$n_country
n_method <- pkg_data$n_method
n_sector <- c("Public", "Commercial_medical", "Other")
n_all_years <- pkg_data$n_years
mydata <- pkg_data$data
all_years <- pkg_data$all_years

# Creating index tables for reference
country_index_table <- tibble::tibble(Country = n_country, index_country = unique(mydata$index_country))
method_index_table <- tibble::tibble(Method = n_method, index_method = 1:length(n_method))
sector_index_table <- tibble::tibble(Sector = n_sector, index_sector = 1:length(n_sector))
year_index_table <- tibble::tibble(average_year = all_years,
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
country_data <- FP_source_data_long[which(FP_source_data_long$Country=="Nepal"), ]
country_calc <- estimates[which(estimates$Country=="Nepal"), ]

ci_plot = ggplot2::ggplot() +  # plot of true p value vs time using facet wrap
    ggplot2::geom_line(data=country_calc %>% dplyr::filter(Method=="Female Sterilization"), ggplot2::aes(x=average_year, y=median_p, color=Sector)) +
    ggplot2::geom_point(data=country_data %>% dplyr::filter(Method=="Female Sterilization"), ggplot2::aes(x=average_year, y=proportion, colour=Sector))+
    ggplot2::geom_errorbar(data=country_data %>% dplyr::filter(Method=="Female Sterilization"), ggplot2::aes(ymin = prop_min, ymax = prop_max, x=average_year, colour=Sector), width = 1.5) +
    ggplot2::geom_ribbon(data=country_calc %>% dplyr::filter(Method=="Female Sterilization"), ggplot2::aes(ymin = lower_95, ymax = upper_95, x=average_year, fill=Sector), alpha=0.2) +
    ggplot2::geom_ribbon(data=country_calc %>% dplyr::filter(Method=="Female Sterilization"), ggplot2::aes(ymin = lower_80, ymax = upper_80, x=average_year, fill=Sector), alpha=0.26) +
    ggplot2::labs(y="", x = "") +
    ggplot2::scale_y_continuous(limits=c(0,1))+
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), strip.text.x = ggplot2::element_text(size = 9)) +
    ggplot2::theme(legend.position = "none")+
    ggplot2::labs(fill = "Sector") +
    ggplot2::guides(color="none") +
    ggplot2::scale_colour_manual(values=safe_colorblind_palette) +
    ggplot2::scale_fill_manual(values=safe_colorblind_palette)

## Loading Google fonts (http://www.google.com/fonts)
fname = "Racing Sans One" #sample(font_families_google(), 100)
font_add_google(fname)
## Automatically use showtext to render text for future devices
showtext_auto()


sticker(ci_plot, package="mcmsupply", p_size=20, s_x=0.95, s_y=1,
        s_width=1.5, s_height=1.1, p_family = fname, h_size = 2,
        p_color = "#56B4E9",
        h_fill="#FFFFFF", h_color="#E69F00", p_x=1, p_y=0.56,
        filename="badge/mcmsupply_badge.png")
