## Load your library
# library(mcmsupply)
devtools::load_all()
library(dplyr)

cleaned_natdata <- get_data(national=FALSE, local=FALSE, mycountry=NULL, fp2030=TRUE)
pkg_data <- get_modelinputs(national=FALSE, local=FALSE, mycountry=NULL, startyear=1990, endyear=2025.5, nsegments=12, raw_data = cleaned_natdata)
run_jags_model(national=FALSE, jagsdata = pkg_data, jagsparams = NULL, local=FALSE, main_path = "results/subnational/", n_iter = 80000, n_burnin = 10000, n_thin = 35, mycountry=NULL)
get_point_estimates(national=FALSE, main_path="results/subnational/", pkg_data, local=FALSE, mycountry=NULL)
plot_point_estimates(national=FALSE, main_path="results/subnational/", vis_path="visualisation/subnational/global/", pkg_data, local=FALSE, mycountry=NULL)
