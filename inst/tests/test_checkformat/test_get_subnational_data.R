library(testthat)
library(mcmsupply)

# library(testthat)
# library(mcmsupply)
# context("Run global subsubnational jags model estimates match")
# test_that("Run jags output matches", {
#   jags_mod <- readRDS("/users/research/hcomiskey/PhD/mcmsupply_results/subnational/global_spatial/P_point_estimates.RDS") %>%
#     dplyr::arrange(Country, Region, Method, Sector, average_year) %>%
#     dplyr::select(Country, Region, Method, Sector, average_year, median_p) %>%
#     dplyr::filter(average_year <= 2022.5)
#
#   org_mod <- readRDS("tests/testthat/orginal_estimates_for_testing_comparision/subnational/global_spatial_p_med.RDS") %>%
#     dplyr::select(Country, Region, Method, Sector, average_year, median_p) %>%
#     dplyr::rename(pred_mu = median_p) %>%
#     dplyr::arrange(Country, Region, Method, Sector, average_year)
#
#   test <- left_join(jags_mod, org_mod) %>%
#     mutate(diff_mcmsupply_original = median_p - pred_mu)
#
#   expect_equal(test$diff_mcmsupply_original, # median estimates should match, then difference should be 0.
#                rep(0, nrow(test)),
#                ignore_attr = FALSE, tolerance = 2e-2) # 2% tolerance
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

# multicountry subnational dataset test
testthat::test_that("Get subnational level single country custom data example", {
  cleaned_subnatdata <- get_data(national=FALSE,
                              local=TRUE,
                              mycountry="Ethiopia",
                              fp2030=TRUE,
                              surveydata_filepath="inst/data-raw/sample_custom_datasets/ethiopia_subnat_customdata.xlsx")
  testthat::expect_true(is.data.frame(cleaned_subnatdata$mydata))
  testthat::expect_true(is.list(cleaned_subnatdata$args))
})


