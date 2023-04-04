library(testthat)
library(mcmsupply)
context("Run local national jags model estimates match")
test_that("Run jags output matches", {
  jags_mod <- readRDS("/users/research/hcomiskey/PhD/mcmsupply_results/national/local/Nepal_P_point_national_estimates.RDS") %>%
    dplyr::arrange(Country, Method, Sector, average_year)

  org_mod <- readRDS("tests/testthat/orginal_estimates_for_testing_comparision/national/Nepal_model_estimates.RDS") %>%
    dplyr::select(Country, Method, Sector, average_year, pred_mu, lwr_95, upr_95) %>%
    dplyr::arrange(Country, Method, Sector, average_year)

  test <- left_join(jags_mod, org_mod) %>%
    mutate(diff_mcmsupply_original = median_p - pred_mu)

  expect_equal(test$diff_mcmsupply_original, # median estimates should match, then difference should be 0.
               rep(0, nrow(test)),
               ignore_attr = FALSE, tolerance = 1e-3)
})
