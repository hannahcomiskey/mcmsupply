library(dplyr)
library(survey)
library(readxl)
library(haven)
library(openxlsx)

# Inputs:
# 1. filepath: The filepath to the .DTA file you wish to process.
# 2. myresultsfolder : A path to the folder containing both the /proportions and the /varcov sub-folders. In these sub-folders you will keep the estimated proportion dataframes and the variance-covariance matrices.
#
# Returns:
# This function will save two dataframes (method supply shares and associated variance-covariance data) to .RDS files in the /proportions and /varcov folders.

calculate_SE_data <- function(filepath, myresultsfolder) {
  vars_needed <- c("v000","v007","v024","v313","v502","v312","v327",
                   "v021","v023","v005")

  data <- haven::read_dta(file = filepath, col_select = all_of(vars_needed))
  year <- min(data$v007)
  survey <- "DHS"
  population <- "AW"
  country_code <- unique(data$v000)

  data$region <- haven::as_factor(data$v024, levels = "labels")

  data <- data %>%
    dplyr::mutate(mcpr = dplyr::case_when(v313 == 3 ~ 1, TRUE ~ 0))

  data <- data %>%
    dplyr::mutate(married = dplyr::case_when(v502 == 1 ~ 1, TRUE ~ 0))

  data <- data %>%
    dplyr::mutate(
      modern_method = dplyr::case_when(
        v312 == 6 ~ "Sterilization (female)",
        v312 == 7 ~ "Sterilization (male)",
        v312 == 2 ~ "IUD",
        v312 == 11 ~ "Implant",
        v312 == 3 ~ "Injectable",
        v312 == 1 ~ "OC Pills",
        v312 == 5 ~ "Condom (m)",
        v312 == 13 ~ "LAM",
        v312 %in% c(4,14,15,16,17,18,19,20) ~ "Other Modern Methods",
        v312 %in% c(0,8,9,10,12) ~ "None"
      )
    )

  data <- data %>%
    dplyr::mutate(
      modern_method_source = dplyr::case_when(
        v312 == 6 ~ "Sterilization (F)",
        v312 == 7 ~ "Sterilization (M)",
        v312 == 2 ~ "IUD",
        v312 == 11 ~ "Implant",
        v312 == 3 ~ "Injectable",
        v312 == 1 ~ "OC Pills",
        v312 == 5 ~ "Condom (M)",
        v312 == 14 ~ "Condom (F)",
        v312 %in% c(4,15,17,18,19,20) ~ "Other Modern Methods",
        v312 == 16 ~ "Emergency contraception",
        v312 %in% c(0,8,9,10,12) ~ "None"
      )
    )

  data <- data %>%
    dplyr::mutate(
      sector = dplyr::case_when(
        v327 %in% c(1,2) ~ "Public",
        v327 == 3 ~ "NGO",
        v327 == 4 ~ "Private Clinic",
        v327 == 5 ~ "Pharmacy",
        v327 == 6 ~ "Shop Church Friend",
        v327 == 7 ~ "Other",
        v327 == 8 ~ NA_character_
      )
    )

  data <- data %>%
    dplyr::mutate(
      sector_categories = dplyr::case_when(
        v327 %in% c(1,2) ~ "Public",
        v327 %in% c(3,4,5) ~ "Commercial_medical",
        v327 %in% c(6,7) ~ "Other",
        v327 == 8 ~ NA_character_
      )
    )

  data$num <- 1
  data$sampleweights <- data$v005 / 1000000

  data <- data %>%
    dplyr::filter(
      !is.na(sector_categories),
      modern_method_source %in% c(
        "Implant", "Injectable", "OC Pills", "Sterilization (F)",
        "Sterilization (M)", "Condom (M)", "Condom (F)", "IUD",
        "Other Modern Methods", "Emergency contraception"
      )
    )

  counts <- data %>%
    dplyr::group_by(sector_categories) %>%
    dplyr::count(modern_method_source) %>%
    dplyr::rename(method = modern_method_source)

  DHSdesign <- survey::svydesign(
    id = ~v021,
    strata = ~v023,
    weights = ~sampleweights,
    data = data,
    nest = TRUE
  )

  options(survey.lonely.psu = "adjust")

  summary(DHSdesign)

  d.s <- update(DHSdesign, modern_method_source = factor(modern_method_source))
  d.s <- update(d.s, sector_categories = factor(sector_categories))

  counts_wide <- counts %>%
    tidyr::pivot_wider(names_from = 'sector_categories',
                       values_from = 'n')

  colnames(counts_wide)[-1] <- paste0(colnames(counts_wide)[-1], '_n')

  prop_mat <- survey::svyby(
    ~I(sector_categories),
    ~I(modern_method_source),
    design = d.s,
    survey::svymean,
    covmat = TRUE
  )

  vcov_matrix <- stats::vcov(prop_mat)

  colnames(prop_mat) <- gsub("I\\(sector_categories\\)", "", colnames(prop_mat))
  colnames(prop_mat) <- gsub("I\\(modern_method_source\\)", "method", colnames(prop_mat))

  prop_mat$country_code <- country_code
  prop_mat$year <- year

  prop_mat <- base::merge(prop_mat, counts_wide)

  openxlsx::write.xlsx(
    prop_mat,
    paste0(myresultsfolder, "/proportions/prop_", country_code, "_", year, "_SEdf.xlsx")
  )

  colnames(vcov_matrix) <- gsub("I\\(sector_categories\\)", "", colnames(vcov_matrix))
  rownames(vcov_matrix) <- gsub("I\\(sector_categories\\)", "", rownames(vcov_matrix))

  vcov_matrix <- tibble::as_tibble(vcov_matrix) %>%
    dplyr::mutate(Method_sector = rownames(vcov_matrix))

  vcov_matrix$country_code <- country_code
  vcov_matrix$year <- year

  openxlsx::write.xlsx(
    vcov_matrix,
    paste0(myresultsfolder, "/varcov/varcov_", country_code, "_", year, "_SEdf.xlsx")
  )
}


# List files in directory
fileloc <- "~/Documents/Family Planning/DTA_files/2025/dta_files/"
fileres <- "~/Documents/R/mcmsupply/inst/data-raw/calculate_supplyshares_from_DTA/"

my_files <- list.files(fileloc) # All files in the directory

# Initialize dud list
dud_list <- vector()

# Required variables
vars_needed <- c("v000","v007","v024","v313","v502","v312","v327",
                 "v021","v023","v005")

for(i in my_files) {
  print(i)
  myfilepath <- file.path(fileloc, i) # Construct full path

  # Read only headers first to check
  file_vars <- tryCatch({
    names(haven::read_dta(myfilepath, n_max = 0))
  }, error = function(e) {
    warning(paste("Failed to read file:", i, " — skipping."))
    dud_list <<- c(dud_list, i)
    return(NULL)
  })

  # If failed to read headers, skip file
  if(is.null(file_vars)) {
    next
  }

  # Check if all required columns are present
  if(!all(vars_needed %in% file_vars)) {
    warning(paste("Missing required columns in file:", i, " — skipping."))
    dud_list <- c(dud_list, i)
    next
  }

  # Try running the function if columns are present
  tryCatch({
    calculate_SE_data(filepath = myfilepath, myresultsfolder = fileres)
  }, error = function(e) {
    warning(paste("Error processing file:", i, " —", conditionMessage(e)))
    dud_list <<- c(dud_list, i)
  })

  # Save dud list after each iteration
  saveRDS(dud_list, file.path(fileres, "dud_jobrun.RDS"))
}
