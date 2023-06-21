# ## Load your library
# # library(mcmsupply)
# devtools::load_all()
# library(dplyr)
#
# cleaned_natdata <- get_data(national=FALSE, local=FALSE, mycountry=NULL, fp2030=TRUE)
# pkg_data <- get_modelinputs(startyear=1990, endyear=2025.5, nsegments=12, raw_data = cleaned_natdata)
# run_jags_model(jagsdata = pkg_data, jagsparams = NULL, main_path = "results/subnational/global/", n_iter = 80000, n_burnin = 10000, n_thin = 35)
# get_point_estimates(main_path="results/subnational/global/", pkg_data)
# plot_point_estimates(main_path="results/subnational/global/", vis_path="visualisation/subnational/global/", pkg_data)
