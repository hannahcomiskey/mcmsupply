library(mcmsupply)
library(dplyr)

cleaned_natdata <- get_national_data(local=FALSE, mycountry=NULL, fp2030=TRUE)

pkg_data <- get_national_modelinputs(fp2030=TRUE, local=FALSE, mycountry=NULL, startyear=1990, endyear=2025.5, nsegments=12, raw_data = cleaned_natdata)

run_national_jags_model(pkg_data = pkg_data, jagsparams = NULL, local=FALSE, main_path = "results/national/", n_iter = 80000, n_burnin = 10000, n_thin = 35, mycountry=NULL)

get_national_P_point_estimates(main_path="results/national/", pkg_data, local=FALSE, mycountry=NULL)

plot_national_point_estimates(main_path="results/national/", vis_path="visualisation/national/global/", pkg_data, local=FALSE, mycountry=NULL)
