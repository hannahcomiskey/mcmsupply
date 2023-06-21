testthat::test_that("Get JAGS model inputs", {
  raw_data <- get_data(national=TRUE,
                              local=TRUE,
                              mycountry="Nepal",
                              fp2030=TRUE,
                              surveydata_filepath=NULL)
  jagsdata <- get_modelinputs(startyear=1990, endyear=2030.5, nsegments=12, raw_data)
  testthat::expect_true(is.list(jagsdata$modelinputs))
  testthat::expect_true(is.list(jagsdata$args))
})

