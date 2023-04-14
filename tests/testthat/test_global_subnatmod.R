## Load your library
# library(mcmsupply)
devtools::load_all()
library(dplyr)

raw_subnatdata <- mcmsupply::get_subnational_data(local=FALSE, mycountry=NULL) %>%
  dplyr::arrange(Country, Region, Method, average_year)
pkg_data <- mcmsupply::get_subnational_modelinputs(fp2030=TRUE, local=FALSE, mycountry=NULL,
                                                   startyear=1990, endyear=2028.5,
                                                   nsegments=12, raw_subnatdata)
run_subnational_jags_model(pkg_data = pkg_data, local=FALSE, main_path = "results/subnational/global/", n_iter = 80000, n_burnin = 10000, n_thin = 35, mycountry=NULL)
get_subnational_P_point_estimates(main_path = "results/subnational/global/", pkg_data, local=FALSE, mycountry=NULL)
plot_subnational_point_estimates(main_path = "results/subnational/global/", pkg_data = pkg_data, vis_path = "visualisation/subnational/global/", local=FALSE, mycountry=NULL)
