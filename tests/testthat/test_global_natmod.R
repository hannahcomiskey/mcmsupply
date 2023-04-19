library(mcmsupply)
library(dplyr)

cleaned_natdata <- get_data(national=TRUE, local=FALSE, mycountry=NULL, fp2030=TRUE)
pkg_data <- get_modelinputs(national=TRUE, local=FALSE, mycountry=NULL, startyear=1990, endyear=2025.5, nsegments=12, raw_data = cleaned_natdata)
run_jags_model(national=TRUE, jagsdata = pkg_data, jagsparams = NULL, local=FALSE, main_path = "results/national/", n_iter = 80000, n_burnin = 10000, n_thin = 35, mycountry=NULL)
get_point_estimates(national=TRUE, main_path="results/national/", pkg_data, local=FALSE, mycountry=NULL)
plot_estimates(national=TRUE, main_path="results/national/", vis_path="visualisation/national/global/", pkg_data, local=FALSE, mycountry=NULL)

