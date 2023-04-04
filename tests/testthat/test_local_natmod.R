devtools::load_all()

cleaned_natdata <- get_national_data(local=TRUE, mycountry="Nepal", fp2030=TRUE)

pkg_data <- get_national_modelinputs(fp2030=TRUE, local=TRUE, mycountry="Nepal", startyear=1990, endyear=2025.5, nsegments=12, raw_data = cleaned_natdata)

run_national_jags_model(pkg_data = pkg_data, local=TRUE, main_path = "results/national/local/", n_iter = 80000, n_burnin = 10000, n_thin = 35, mycountry="Nepal")

get_national_P_point_estimates(main_path="results/national/local/", pkg_data, local=TRUE, mycountry="Nepal")

plot_national_point_estimates (main_path="results/national/local/", vis_path="visualisation/national/local/", pkg_data, local=TRUE, mycountry="Nepal")
