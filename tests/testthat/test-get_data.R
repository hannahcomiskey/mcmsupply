# Single country national dataset test
testthat::test_that("Get national level single country data example", {
  cleaned_natdata <- get_data(national=TRUE,
                              local=TRUE,
                              mycountry="Nepal",
                              fp2030=TRUE,
                              surveydata_filepath=NULL)
  testthat::expect_true(is.data.frame(cleaned_natdata$mydata))
  testthat::expect_true(is.list(cleaned_natdata$args))
})

# multicountry national dataset test
testthat::test_that("Get national level single country data example", {
  cleaned_natdata <- get_data(national=TRUE,
                              local=FALSE,
                              mycountry=NULL,
                              fp2030=TRUE)
  testthat::expect_true(is.data.frame(cleaned_natdata$mydata))
  testthat::expect_true(is.list(cleaned_natdata$args))
})

# # multicountry national dataset test
# testthat::test_that("Get national level single country custom data example", {
#   cleaned_natdata <- get_data(national=TRUE,
#                               local=TRUE,
#                               mycountry="Afghanistan",
#                               fp2030=TRUE,
#                               surveydata_filepath="inst/data-raw/sample_custom_datasets/national_user_input_test_correct.xlsx")
#   testthat::expect_true(is.data.frame(cleaned_natdata$mydata))
#   testthat::expect_true(is.list(cleaned_natdata$args))
# })

# Single country subnational dataset test
testthat::test_that("Get subnational level single country data example", {
  cleaned_subnatdata <- get_data(national=FALSE,
                                 local=TRUE,
                                 mycountry="Nepal",
                                 fp2030=TRUE,
                                 surveydata_filepath=NULL)
  testthat::expect_true(is.data.frame(cleaned_subnatdata$mydata))
  testthat::expect_true(is.list(cleaned_subnatdata$args))
})

# Multicountry subnational dataset test
testthat::test_that("Get subnational level single country data example", {
  cleaned_subnatdata <- get_data(national=FALSE,
                                 local=FALSE,
                                 mycountry=NULL,
                                 fp2030=TRUE)
  testthat::expect_true(is.data.frame(cleaned_subnatdata$mydata))
  testthat::expect_true(is.list(cleaned_subnatdata$args))
})

# # multicountry subnational dataset test
# testthat::test_that("Get subnational level single country custom data example", {
#   cleaned_subnatdata <- get_data(national=FALSE,
#                                  local=TRUE,
#                                  mycountry="Ethiopia",
#                                  fp2030=TRUE,
#                                  surveydata_filepath="inst/data-raw/sample_custom_datasets/ethiopia_subnat_customdata.xlsx")
#   testthat::expect_true(is.data.frame(cleaned_subnatdata$mydata))
#   testthat::expect_true(is.list(cleaned_subnatdata$args))
# })


