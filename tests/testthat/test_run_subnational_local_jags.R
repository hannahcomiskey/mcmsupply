library(testthat)
library(mcmsupply)
context("Run local subnational jags model estimates match")
test_that("Run jags output matches", {
  jags_mod <- readRDS("/users/research/hcomiskey/PhD/mcmsupply_results/subnational/local_spatial/Nepal_P_point_estimates.RDS") %>%
    dplyr::arrange(Country, Region, Method, Sector, average_year) %>%
    dplyr::select(Country, Region, Method, Sector, average_year, median_p) %>%
    dplyr::filter(average_year <= 2022.5)

  org_mod <- readRDS("tests/testthat/orginal_estimates_for_testing_comparision/subnational/Nepal_pred_summary_spatial_spatialcovar_t25.RDS") %>%
    dplyr::select(Region, Method, Sector, average_year, median_p) %>%
    dplyr::rename(pred_mu = median_p) %>%
    dplyr::arrange(Region, Method, Sector, average_year)

  test <- left_join(jags_mod, org_mod) %>%
    mutate(diff_mcmsupply_original = median_p - pred_mu)

  expect_equal(test$diff_mcmsupply_original, # median estimates should match, then difference should be 0.
               rep(0, nrow(test)),
               ignore_attr = FALSE, tolerance = 1e-2) # 1% tolerance
})
