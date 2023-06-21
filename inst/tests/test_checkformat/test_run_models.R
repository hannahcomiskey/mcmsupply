library(testthat)
library(mcmsupply)

# Testing the one-country national model
testthat::test_that("Testing MCMC for single-country national model", {
  cleaned_natdata <- get_data(national=TRUE,
                              local=TRUE,
                              mycountry="Nepal",
                              fp2030=TRUE,
                              surveydata_filepath=NULL)
  pkg_data <- get_modelinputs(startyear=1990,
                              endyear=2025.5,
                              nsegments=12,
                              raw_data = cleaned_natdata)
  mod <- run_jags_model(jagsdata = pkg_data,
                 jagsparams = NULL,
                 main_path = "tests/testthat/test_checkformat/",
                 n_iter = 10,
                 n_burnin = 1,
                 n_thin = 1)

  testthat::expect_true(is.list(mod$model))
  testthat::expect_true(is.matrix(mod$BUGSoutput$sims.matrix))
  testthat::expect_true(is.character(mod$parameters.to.save))
})


# Testing the multi-country national model
testthat::test_that("Testing MCMC for multi-country national model", {
  cleaned_natdata <- get_data(national=TRUE,
                              local=FALSE,
                              mycountry=NULL,
                              fp2030=TRUE)
  pkg_data <- get_modelinputs(startyear=1990,
                              endyear=2025.5,
                              nsegments=12,
                              raw_data = cleaned_natdata)
  mod <- run_jags_model(jagsdata = pkg_data,
                        jagsparams = NULL,
                        main_path = "tests/testthat/test_checkformat/",
                        n_iter = 10,
                        n_burnin = 1,
                        n_thin = 1)

  testthat::expect_true(is.list(mod$model))
  testthat::expect_true(is.matrix(mod$BUGSoutput$sims.matrix))
  testthat::expect_true(is.character(mod$parameters.to.save))
})


# Testing the one-country subnational model
testthat::test_that("Testing MCMC for single-country subnational model", {
  cleaned_natdata <- get_data(national=FALSE,
                              local=TRUE,
                              mycountry="Nepal",
                              fp2030=TRUE,
                              surveydata_filepath=NULL)
  pkg_data <- get_modelinputs(startyear=1990,
                              endyear=2025.5,
                              nsegments=12,
                              raw_data = cleaned_natdata)
  mod <- run_jags_model(jagsdata = pkg_data,
                        jagsparams = NULL,
                        main_path = "tests/testthat/test_checkformat/",
                        n_iter = 10,
                        n_burnin = 1,
                        n_thin = 1)

  testthat::expect_true(is.list(mod$model))
  testthat::expect_true(is.matrix(mod$BUGSoutput$sims.matrix))
  testthat::expect_true(is.character(mod$parameters.to.save))
})


# Testing the multi-country subnational model
testthat::test_that("Testing MCMC for multi-country subnational model", {
  cleaned_natdata <- get_data(national=FALSE,
                              local=FALSE,
                              mycountry=NULL,
                              fp2030=TRUE)
  pkg_data <- get_modelinputs(startyear=1990,
                              endyear=2025.5,
                              nsegments=12,
                              raw_data = cleaned_natdata)
  mod <- run_jags_model(jagsdata = pkg_data,
                        jagsparams = NULL,
                        main_path = "tests/testthat/test_checkformat/",
                        n_iter = 10,
                        n_burnin = 1,
                        n_thin = 1)

  testthat::expect_true(is.list(mod$model))
  testthat::expect_true(is.matrix(mod$BUGSoutput$sims.matrix))
  testthat::expect_true(is.character(mod$parameters.to.save))
})
