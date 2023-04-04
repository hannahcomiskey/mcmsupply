
library(mcmsupply)
library(dplyr)

raw_subnatdata <- get_subnational_data(local=TRUE, mycountry="Nepal") %>%
  dplyr::arrange(Country, Region, Method, average_year)

pkg_data <- get_subnational_modelinputs(fp2030=TRUE, local=TRUE,
                                                   spatial=FALSE, mycountry="Nepal",
                                                   startyear=1990, endyear=2028.5,
                                                   nsegments=12, raw_subnatdata)

run_subnational_jags_model(pkg_data = pkg_data, jagsparams = NULL, local=TRUE, spatial=FALSE, main_path = "results/subnational/local_nonspatial/", n_iter = 80000, n_burnin = 10000, n_thin = 35, mycountry="Nepal")

get_subnational_P_point_estimates(main_path = "results/subnational/local_nonspatial/", pkg_data, local=TRUE, spatial=FALSE, mycountry="Nepal")

plot_subnational_point_estimates(main_path = "results/subnational/local_nonspatial/", pkg_data = pkg_data, vis_path = "visualisation/subnational/local_nonspatial/", local=TRUE, mycountry="Nepal")

