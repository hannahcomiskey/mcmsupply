# devtools::load_all()
#
# cleaned_natdata <- get_data(national=FALSE, local=TRUE, mycountry="Nepal", fp2030=TRUE)
# pkg_data <- get_modelinputs(startyear=1990, endyear=2025.5, nsegments=12, raw_data = cleaned_natdata)
# run_jags_model(jagsdata = pkg_data, jagsparams = NULL, main_path = "results/subnational/local/", n_iter = 80000, n_burnin = 10000, n_thin = 35)
# get_point_estimates(main_path="results/subnational/local/", pkg_data)
# plot_estimates(main_path="results/subnational/local/", vis_path="visualisation/subnational/local/", pkg_data)
