library(testthat)
library(mcmsupply)
context("Run global national jags model estimates match")
test_that("Run jags output matches", {
  jags_mod <- readRDS("/users/research/hcomiskey/PhD/mcmsupply_results/national/P_point_national_estimates.RDS") %>%
    dplyr::arrange(Country, Method, Sector, average_year)

  org_mod <- read_csv("tests/testthat/orginal_estimates_for_testing_comparision/national/national_model_calculations_df.csv") %>%
    dplyr::select(Country, Method, sector_category, average_year, median_p, lower_q_P, upper_q_P) %>%
    dplyr::rename(pred_mu = median_p, Sector = sector_category) %>%
    dplyr::arrange(Country, Method, Sector, average_year)

  test <- left_join(jags_mod, org_mod) %>%
    mutate(diff_mcmsupply_original = median_p - pred_mu)

  expect_equal(test$diff_mcmsupply_original, # median estimates should match, then difference should be 0.
               rep(0, nrow(test)),
               ignore_attr = FALSE, tolerance = 1e-2) # 1% tolerance
})
