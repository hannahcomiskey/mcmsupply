devtools::load_all()

cleaned_natdata <- get_data(national=FALSE, local=TRUE, mycountry="Nepal", fp2030=TRUE)
pkg_data <- get_modelinputs(national=FALSE, local=TRUE, mycountry="Nepal", startyear=1990, endyear=2025.5, nsegments=12, raw_data = cleaned_natdata)
run_jags_model(national=FALSE, jagsdata = pkg_data, jagsparams = NULL, local=TRUE, main_path = "results/subnational/", n_iter = 80000, n_burnin = 10000, n_thin = 35, mycountry="Nepal")
get_point_estimates(national=FALSE, main_path="results/subnational/", pkg_data, local=TRUE, mycountry="Nepal")
plot_point_estimates(national=FALSE, main_path="results/subnational/", vis_path="visualisation/subnational/local/", pkg_data, local=TRUE, mycountry="Nepal")
