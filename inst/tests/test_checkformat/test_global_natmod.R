# library(mcmsupply)
# library(dplyr)
#
# cleaned_natdata <- get_data(national=TRUE, local=FALSE, mycountry=NULL, fp2030=TRUE)
# pkg_data <- get_modelinputs(startyear=1990, endyear=2025.5, nsegments=12, raw_data = cleaned_natdata)
# run_jags_model(jagsdata = pkg_data, jagsparams = NULL, main_path = "results/national/", n_iter = 80000, n_burnin = 10000, n_thin = 35)
# get_point_estimates(main_path="results/national/", pkg_data)
# plot_estimates(main_path="results/national/", vis_path="visualisation/national/global/", pkg_data)
#
