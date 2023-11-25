# Limit cores for CRAN
options(mc.cores=2)

# Single country national dataset test
# testthat::test_that("Get national level single country data example", {
#   raw_data <- get_data(national=TRUE, local=TRUE, mycountry="Nepal")
#   jagsdata <- get_modelinputs(startyear=1990, endyear=2020.5, nsegments=12, raw_data)
#   mod <- run_jags_model(jagsdata = jagsdata, jagsparams = NULL, n_iter = 5, n_burnin = 1, n_thin = 1)
#   estimates <- pull_estimates(model_output = mod, year=2020, country='Benin')
#   testthat::expect_true(is.matrix(estimates))
# })
