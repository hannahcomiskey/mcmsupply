# Limit cores for CRAN
options(mc.cores=2)

# testthat::test_that("Get posterior samples of national level estimates", {
  raw_data <- get_data(national=TRUE, local=TRUE, mycountry="Nepal")
  jagsdata <- get_modelinputs(startyear=1990, endyear=2025.5, nsegments=12, raw_data)
  mod <- run_jags_model(jagsdata = jagsdata, jagsparams = NULL, n_iter = 5, n_burnin = 1, n_thin = 1)
  post_samps <- get_posterior_P_samps(jagsdata = jagsdata, model_output = mod, nposterior=4)
  testthat::expect_true(is.data.frame(post_samps))
# })
