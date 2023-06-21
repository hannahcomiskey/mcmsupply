# ### How to run a subnational level model for a single country with custom data
# devtools::load_all()
# library(dplyr)
# cleaned_data <- get_data(national=FALSE, local=TRUE, mycountry="Congo Democratic Republic", fp2030=TRUE, surveydata_filepath="data-raw/sample_custom_datasets/DRC_admin2_FPsource.xlsx")
# pkg_data <- get_modelinputs(startyear=1990, endyear=2025.5, nsegments=12, raw_data = cleaned_data)
# run_jags_model(jagsdata = pkg_data, jagsparams = NULL, main_path = "results/subnational/local/test/", n_iter = 80000, n_burnin = 10000, n_thin = 35)
# plot_estimates(main_path="results/subnational/local/test/", vis_path="visualisation/subnational/local/test/", pkg_data)
#
