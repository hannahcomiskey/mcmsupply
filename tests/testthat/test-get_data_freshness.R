testthat::test_that("check_data_freshness works with basic inputs", {
  # Create mock local data
  local_data <- data.frame(
    Country = c("Kenya", "Uganda", "Tanzania"),
    Year = c(2020, 2019, 2018)
  )
  # Create mock reference data
  reference_data <- data.frame(
    Country = c("Kenya", "Kenya", "Uganda", "Uganda", "Tanzania", "Tanzania"),
    Year = c(2020, 2022, 2019, 2021, 2018, 2020),
    `Survey Datasets` = c("Data Available", "Data Available",
                          "Data Available", "Survey in Progress",
                          "Data Available", "Data Available"),
    check.names = FALSE
  )
  result <- check_data_freshness(
    local = local_data,
    reference = reference_data
  )
  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(all(c("Country", "Year.package", "Year.most_recent",
                              "Year.most_recent_available", "Status") %in% colnames(result)))
  # Check number of rows matches number of countries
  testthat::expect_equal(nrow(result), 3)
  # Check Status is a factor
  testthat::expect_s3_class(result$Status, "factor")
})
