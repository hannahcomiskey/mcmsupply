devtools::load_all()

cleaned_natdata <- get_data(national=TRUE, local=TRUE, mycountry="Nepal", fp2030=TRUE)
pkg_data <- get_modelinputs(national=TRUE, local=TRUE, mycountry="Nepal", startyear=1990, endyear=2025.5, nsegments=12, raw_data = cleaned_natdata)
run_jags_model(national=TRUE, jagsdata = pkg_data, jagsparams = NULL, local=TRUE, main_path = "results/national/", n_iter = 80000, n_burnin = 10000, n_thin = 35, mycountry="Nepal")
get_point_estimates(national=TRUE, main_path="results/national/", pkg_data, local=TRUE, mycountry="Nepal")
plot_estimates(national=TRUE, main_path="results/national/", vis_path="visualisation/national/local/", pkg_data, local=TRUE, mycountry="Nepal")
