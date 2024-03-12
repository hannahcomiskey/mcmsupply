# Limit cores for CRAN
options(mc.cores=2)

# Single country national dataset test
testthat::test_that("Get national level single country data example", {
  raw_data <- get_data(national=TRUE, local=TRUE, mycountry="Nepal")
  jagsdata <- get_modelinputs(startyear=1990, endyear=2025.5, nsegments=12, raw_data)
  mod <- run_jags_model(jagsdata = jagsdata, jagsparams = NULL, n_iter = 5, n_burnin = 1, n_thin = 1)
  plots <- plot_estimates(jagsdata = jagsdata, model_output = mod)
  testthat::expect_true(is.list(plots))
  testthat::expect_true(is.list(mod$JAGS$model))
  testthat::expect_true(is.matrix(mod$JAGS$BUGSoutput$sims.matrix))
  testthat::expect_true(is.character(mod$JAGS$parameters.to.save))
  testthat::expect_true(is.data.frame(mod$estimates))
})

# # multicountry national dataset test
# testthat::test_that("Get national level single country data example", {
#   raw_data <- get_data(national=TRUE,
#                               local=FALSE,
#                               mycountry=NULL,
#                               fp2030=TRUE)
#   jagsdata <- get_modelinputs(startyear=1990, endyear=2030.5, nsegments=12, raw_data)
#   mod <- run_jags_model(jagsdata = jagsdata, jagsparams = NULL, n_iter = 5, n_burnin = 1, n_thin = 1, n_chain=1)
#   testthat::expect_true(is.list(mod$JAGS$model))
#   testthat::expect_true(is.matrix(mod$JAGS$BUGSoutput$sims.matrix))
#   testthat::expect_true(is.character(mod$JAGS$parameters.to.save))
#   testthat::expect_true(is.data.frame(mod$estimates))
# })

# # multicountry national dataset test
# testthat::test_that("Get national level single country custom data example", {
#   raw_data <- get_data(national=TRUE,
#                               local=TRUE,
#                               mycountry="Afghanistan",
#                               fp2030=TRUE,
#                               surveydata_filepath="inst/data-raw/sample_custom_datasets/national_user_input_test_correct.xlsx")
#   pkg_data <- get_modelinputs(startyear=1990, endyear=2030.5, nsegments=12, raw_data)
#   mod <- run_jags_model(jagsdata = pkg_data, jagsparams = NULL, n_iter = 10, n_burnin = 1, n_thin = 1)
#   testthat::expect_true(is.list(mod$model))
#   testthat::expect_true(is.matrix(mod$BUGSoutput$sims.matrix))
#   testthat::expect_true(is.character(mod$parameters.to.save))
# })

# # Single country subnational dataset test
# testthat::test_that("Get subnational level single country data example", {
#   raw_data <- get_data(national=FALSE, local=TRUE, mycountry="Nepal")
#   jagsdata <- get_modelinputs(startyear=1990, endyear=2030.5, nsegments=12, raw_data)
#   mod <- run_jags_model(jagsdata = jagsdata, jagsparams = NULL,  n_iter = 10, n_burnin = 1, n_thin = 1, n_chain=1, n_cores=2)
#   testthat::expect_true(is.list(mod$JAGS$model))
#   testthat::expect_true(is.matrix(mod$JAGS$BUGSoutput$sims.matrix))
#   testthat::expect_true(is.character(mod$JAGS$parameters.to.save))
#   testthat::expect_true(is.data.frame(mod$estimates))
# })

# # Multicountry subnational dataset test
# testthat::test_that("Get subnational level multi-country data example", {
#   raw_data <- get_data(national=FALSE,
#                                  local=FALSE,
#                                  mycountry=NULL,
#                                  fp2030=TRUE)
#   jagsdata <- get_modelinputs(startyear=1990, endyear=2030.5, nsegments=12, raw_data)
#   mod <- run_jags_model(jagsdata = jagsdata, jagsparams = NULL, n_iter = 5, n_burnin = 1, n_thin = 1, n_chain=1)
#   testthat::expect_true(is.list(mod$JAGS$model))
#   testthat::expect_true(is.matrix(mod$JAGS$BUGSoutput$sims.matrix))
#   testthat::expect_true(is.character(mod$JAGS$parameters.to.save))
#   testthat::expect_true(is.data.frame(mod$estimates))
# })

# single country custom data subnational test
# testthat::test_that("Get subnational level single country custom data example", {
#   raw_data <- get_data(national=FALSE,
#                                  local=TRUE,
#                                  mycountry="Ethiopia",
#                                  fp2030=TRUE,
#                                  surveydata_filepath="inst/data-raw/sample_custom_datasets/ethiopia_subnat_customdata.xlsx")
#   pkg_data <- get_modelinputs(startyear=1990, endyear=2030.5, nsegments=12, raw_data)
#   mod <- run_jags_model(jagsdata = pkg_data, jagsparams = NULL, main_path = "tests/testthat/", n_iter = 100, n_burnin = 10, n_thin = 1)
#   testthat::expect_true(is.array(mod$BUGSoutput$sims.array))
#   testthat::expect_true(is.character(mod$parameters.to.save))
# })


