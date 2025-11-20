#' Calculate Standard Errors and Variance-Covariance Matrices for DHS Subnational Estimates
#'
#' This function reads a DHS dataset, constructs a survey design object,
#' computes weighted estimates of modern contraceptive method sources by
#' sector category, and exports both the proportion matrix and the
#' variance-covariance matrix to Excel files.
#'
#' @param filepath Character string. The path to the DHS `.dta` file.
#' @param myresultsfolder Character string. Path to a folder where Excel
#'   outputs will be saved. The function will create the required
#'   subfolders (`proportions/` and `varcov/`) if they do not exist.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Reads DHS survey microdata.
#'   \item Constructs derived variables such as modern method, sector,
#'     and method source.
#'   \item Builds a survey design using \pkg{survey}.
#'   \item Computes weighted proportions using \code{svyby()}.
#'   \item Extracts the associated variance–covariance matrix.
#'   \item Exports both datasets as Excel workbooks.
#' }
#'
#' @return
#' A list with two elements (returned invisibly):
#' \describe{
#'   \item{prop_mat}{Data frame of weighted estimates with counts}
#'   \item{vcov_matrix}{Variance–covariance matrix tibble}
#' }
#' Output files are written to disk as a side effect.
#'
#' @importFrom haven read_dta as_factor
#' @importFrom dplyr mutate case_when filter group_by count rename %>%
#' @importFrom tidyr pivot_wider
#' @importFrom survey svydesign svyby svymean
#' @importFrom stats vcov
#' @importFrom tibble as_tibble
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' \dontrun{
#' calculate_SE_data("data/country.dta", "results/")
#' }
#'
#' @export
calculate_SE_data_from_DTA <- function(filepath, myresultsfolder) {

  ## --- Input checks ---
  if (!file.exists(filepath)) {
    stop("The file ", filepath, " does not exist.")
  }

  if (!dir.exists(myresultsfolder)) {
    stop("The folder ", myresultsfolder, " does not exist.")
  }

  if (!dir.exists(file.path(myresultsfolder, "proportions"))) {
    dir.create(file.path(myresultsfolder, "proportions"), recursive = TRUE)
  }
  if (!dir.exists(file.path(myresultsfolder, "varcov"))) {
    dir.create(file.path(myresultsfolder, "varcov"), recursive = TRUE)
  }

  ## --- Read data ---
  data <- haven::read_dta(filepath)
  year <- min(data$v007)
  country_code <- unique(data$v000)

  ## --- Derived variables ---
  data$region <- haven::as_factor(data$v024, levels = "labels")

  data <- data %>%
    dplyr::mutate(
      mcpr    = dplyr::case_when(v313 == 3 ~ 1, TRUE ~ 0),
      married = dplyr::case_when(v502 == 1 ~ 1, TRUE ~ 0),
      modern_method = dplyr::case_when(
        v312 == 6  ~ "Sterilization (female)",
        v312 == 7  ~ "Sterilization (male)",
        v312 == 2  ~ "IUD",
        v312 == 11 ~ "Implant",
        v312 == 3  ~ "Injectable",
        v312 == 1  ~ "OC Pills",
        v312 == 5  ~ "Condom (m)",
        v312 == 13 ~ "LAM",
        v312 %in% c(4,14,15,16,17,18,19,20) ~ "Other Modern Methods",
        v312 %in% c(0,8,9,10,12) ~ "None"
      ),
      modern_method_source = dplyr::case_when(
        v312 == 6  ~ "Sterilization (F)",
        v312 == 7  ~ "Sterilization (M)",
        v312 == 2  ~ "IUD",
        v312 == 11 ~ "Implant",
        v312 == 3  ~ "Injectable",
        v312 == 1  ~ "OC Pills",
        v312 == 5  ~ "Condom (M)",
        v312 == 14 ~ "Condom (F)",
        v312 %in% c(4,15,17,18,19,20) ~ "Other Modern Methods",
        v312 == 16 ~ "Emergency contraception",
        v312 %in% c(0,8,9,10,12) ~ "None"
      ),
      sector_categories = dplyr::case_when(
        v327 %in% c(1,2) ~ "Public",
        v327 %in% c(3,4,5) ~ "Commercial_medical",
        v327 %in% c(6,7) ~ "Other",
        v327 == 8 ~ NA_character_
      ),
      sampleweights = v005 / 1e6
    ) %>%
    dplyr::filter(
      !is.na(sector_categories),
      modern_method_source %in% c(
        "Implant", "Injectable", "OC Pills", "Sterilization (F)",
        "Sterilization (M)", "Condom (M)", "Condom (F)", "IUD",
        "Other Modern Methods", "Emergency contraception"
      )
    )

  ## --- Counts for later merging ---
  counts <- data %>%
    dplyr::group_by(sector_categories) %>%
    dplyr::count(modern_method_source) %>%
    dplyr::rename(method = modern_method_source)

  ## --- Survey design ---
  DHSdesign <- survey::svydesign(
    id = ~v021,
    strata = ~v023,
    weights = ~sampleweights,
    data = data,
    nest = TRUE
  )

  options(survey.lonely.psu = "adjust")

  d.s <- survey::update(DHSdesign,
                        modern_method_source = factor(modern_method_source),
                        sector_categories = factor(sector_categories))

  ## --- Counts wide ---
  counts_wide <- counts %>%
    tidyr::pivot_wider(names_from = sector_categories, values_from = n)

  colnames(counts_wide)[-1] <- paste0(colnames(counts_wide)[-1], "_n")

  ## --- Proportions & variance-covariance ---
  prop_mat <- survey::svyby(
    ~I(sector_categories),
    ~I(modern_method_source),
    design = d.s,
    survey::svymean,
    covmat = TRUE
  )

  vcov_matrix <- stats::vcov(prop_mat)

  ## --- Clean proportions ---
  colnames(prop_mat) <- gsub("I\\(sector_categories\\)", "", colnames(prop_mat))
  colnames(prop_mat) <- gsub("I\\(modern_method_source\\)", "method", colnames(prop_mat))

  prop_mat$country_code <- country_code
  prop_mat$year <- year

  prop_mat <- merge(prop_mat, counts_wide)

  ## --- Export proportions ---
  openxlsx::write.xlsx(
    prop_mat,
    file.path(myresultsfolder, "proportions",
              paste0("prop_", country_code, "_", year, "_SEdf.xlsx"))
  )

  ## --- Clean VCOV ---
  colnames(vcov_matrix) <- gsub("I\\(sector_categories\\)", "", colnames(vcov_matrix))
  rownames(vcov_matrix) <- gsub("I\\(sector_categories\\)", "", rownames(vcov_matrix))

  vcov_matrix <- tibble::as_tibble(vcov_matrix) %>%
    dplyr::mutate(Method_sector = rownames(vcov_matrix),
                  country_code = country_code,
                  year = year)

  ## --- Export VCOV ---
  openxlsx::write.xlsx(
    vcov_matrix,
    file.path(myresultsfolder, "varcov",
              paste0("varcov_", country_code, "_", year, "_SEdf.xlsx"))
  )

  ## Return silently
  invisible(list(
    prop_mat = prop_mat,
    vcov_matrix = vcov_matrix
  ))
}
