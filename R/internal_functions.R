#' Get precisely aligned basis functions
#' @name bs_bbase_precise
#' @param x The vector of years you wish to create your basis functions over
#' @param lastobs The year of the most recent survey you wish to align the knots with. Default is max(x).
#' @param xl Default is xl = min(x)
#' @param xr Default is xr = max(x)
#' @param nseg Number of knots you wish to use
#' @param deg The degree of the polynomial. Default is 3.
#' @return B.ik is a matrix, each row is one observation, each column is one B-spline.
#' knots.k is a vector of transformed knots.
#' Kstar is the knot point of last observation
#' @noRd

bs_bbase_precise <- function(x = x,lastobs = max(x), xl = min(x), xr = max(x), nseg = nseg, deg = 3) {
  # Compute the length of the partitions
  dx <- (xr - xl) / nseg
  # Compute position of knot before last observation
  dk <- lastobs
  # Create equally spaced knots
  knots <- seq(xl - deg * dx, xr + deg * dx, by = dx)
  # Find index of closest knot to dk
  dk_index <- which.min(abs(knots-dk))
  # Find transformation to knot placement so that dk is a knot
  ktrans <- (dk-knots)[dk_index]
  # Add transformation to knots
  knotsnew <- knots + ktrans
  # Use bs() function to generate the B-spline basis
  get_bs_matrix <- matrix(splines::bs(x, knots = knotsnew, degree = deg, Boundary.knots = c(knotsnew[1], knotsnew[length(knotsnew)])), nrow = length(x))

  # Remove columns that contain zero only
  bs_matrix <- get_bs_matrix[, -c(1:deg, ncol(get_bs_matrix):(ncol(get_bs_matrix) - deg))]

  used_knots <- knotsnew[-c(1,2,length(knotsnew),(length(knotsnew)-1))]
  Kstar <- which(used_knots==dk)

  return(list(B.ik = bs_matrix, ##<< Matrix, each row is one observation, each column is one B-spline.
              knots.k = used_knots, ##<< Vector of transformed knots.
              Kstar = Kstar # Knot point of last observation
  ))
}

#' Check format of data
#' @name check_format
#' @param format_list The list of requirements for the data type (national or subnational). See the /data folder to review the _format files.
#' @param data The data to be checked.
#' @return An informative error message if data does not pass validation
#' @source With thanks, taken from https://github.com/AlkemaLab/fpemlocal/blob/master/R/format_check.R
#' @noRd

check_format <- function(format_list, data) {
  error_vector <- c()
  for (name in names(format_list)) {
    if (format_list[[name]][["required"]] & (!name %in% names(data))) { #if not required and not in data iteration continues
      error_vector <- paste0(error_vector, "DATA FORMAT ERROR: Column ", name," is missing or incorrectly named. ")
    }
    if (name %in% names(data)) { #if not in data we do not check format
      #check for missing values if they are not supported
      if(!"basic" %in% names(format_list[[name]])) {
        if (any(is.na(data[[name]])) & !format_list[[name]][["missing"]]) {
          error_vector <- paste0(error_vector, "DATA FORMAT ERROR: Column" , name," has missing values. Missing values not supported for this column. ")
        }
        #if missing values are supported we need to exclude them to past the next set of tests
        clean_column <- data[[name]] %>% drop_na()
        if (format_list[[name]][["type"]] == "range") {
          if (!(all(clean_column >= min(format_list[[name]][["valid"]])) &  all(clean_column <= max(format_list[[name]][["valid"]])))) {
            error_vector <- paste0(error_vector, "DATA FORMAT ERROR: Column ", name, " has one or more values which fall outside of valid range. ")
          }
        }
        if (format_list[[name]][["type"]] == "value") {
          if (!all(clean_column %in% format_list[[name]][["valid"]])) {
            error_vector <- paste0(error_vector, "DATA FORMAT ERROR: Column ", name, " has one or more values which are not allowed. ")
          }
        }
      }
    } # end what is done if column is found in user data
  }# end of loop through all columns
  if (!is.null(error_vector)) {
    stop(error_vector)
  }
} # end function

#' Standardise subnational region names over time
#' @name clean_subnat_names
#' @param fp2030 TRUE/FALSE. Default is TRUE. Filters the data to only include FP2030 countries.
#' @param raw_subnatdata The subnational family planning source data from the `mcmsupply::get_subnational_data()` function.
#' @return A dataset with the subnational regions names cleaned for Rwanda, Nigeria and Cote d'Ivoire. Optional to filter for only FP2030 countries.
#' @noRd

clean_subnat_names <- function(fp2030=TRUE, raw_subnatdata) {
  if(fp2030==TRUE) {
    FP_2030_countries <- c("Afghanistan","Benin","Burkina Faso","Cameroon",
                           "Congo", "Congo Democratic Republic", "Cote d'Ivoire",
                           "Ethiopia", "Ghana","Guinea","India","Kenya", "Liberia", "Madagascar",
                           "Malawi","Mali", "Mozambique", "Myanmar", "Nepal", "Niger", "Nigeria", "Pakistan",
                           "Philippines", "Rwanda", "Senegal", "Sierra Leone", "Togo", "Tanzania", "Uganda", "Zimbabwe")   # Using only FPET countries for now

    raw_subnatdata <- raw_subnatdata %>%
      dplyr::filter(Country %in% FP_2030_countries)
  }

  area_classification <- mcmsupply::Country_and_area_classification %>%
    dplyr::select(`Country or area`, Region) %>%
    dplyr::rename(Country = `Country or area`) %>%
    dplyr::rename(Super_region = Region)

  FP_source_data_wide <- raw_subnatdata %>%
    dplyr::left_join(area_classification)

  FP_source_subset <- FP_source_data_wide %>%   # Replace issues with Rwanda names
    dplyr::filter(Country=="Rwanda") %>%
    dplyr::mutate(Region = dplyr::case_when(Region == "Ouest" ~ "West",
                                            Region == "Sud" ~ "South",
                                            Region == "Est" ~ "East",
                                            Region == "Ville de Kigali" ~ "Kigali",
                                            Region == "Kigali City" ~ "Kigali",
                                            TRUE ~ as.character(Region))) %>%
    dplyr::filter(average_year > 2008) # removes old regions

  FP_source_data_wide <- FP_source_data_wide %>%
    dplyr::filter(Country!="Rwanda")

  FP_source_data_wide <- FP_source_data_wide %>%
    merge(FP_source_subset, all = TRUE)

  # Replace issues with Nigeria names
  FP_source_subset <- FP_source_data_wide %>%
    dplyr::filter(Country=="Nigeria") %>%
    dplyr::mutate(Region = dplyr::case_when(Region == "Northeast" ~ "North East",
                                            Region == "Northwest" ~ "North West",
                                            Region == "Southeast" ~ "South East",
                                            Region == "Southwest" ~ "South West",
                                            TRUE ~ as.character(Region)))

  FP_source_data_wide <- FP_source_data_wide %>%
    dplyr::filter(Country!="Nigeria")

  FP_source_data_wide <- FP_source_data_wide %>%
    merge(FP_source_subset, all = TRUE)

  # Replace issues with Cameroon names
  FP_source_subset <- FP_source_data_wide %>%
    dplyr::filter(Country=="Cote d'Ivoire") %>%
    dplyr::mutate(Region = dplyr::case_when(Region == "Center-East" ~ "Center East",
                                            Region == "Center-North" ~ "Center North",
                                            Region == "Center-West" ~ "Center West",
                                            Region == "Center-South" ~ "Center South",
                                            TRUE ~ as.character(Region)))

  FP_source_data_wide <- FP_source_data_wide %>%
    dplyr::filter(Country!="Cote d'Ivoire")

  FP_source_data_wide <- FP_source_data_wide %>%
    merge(FP_source_subset, all = TRUE)

  return(FP_source_data_wide)
}

#' Indexing function for countries used in data.
#' @name country_index_fun
#' @param my_data The data with a Country column you want to index.
#' @param  my_countries A vector of country names used in the data
#' @return returns table with countries indexed
#' @noRd

country_index_fun <- function(my_data, my_countries) {
  my_data$index_country <- NA
  for (i in 1:length(my_countries)) {
    for (j in 1:nrow(my_data)) {
      country_name <- my_countries[i]
      if(my_data$Country[j]==country_name) {
        my_data$index_country[j] <- i
      }
      else {
        next
      }
    }
  }
  return(my_data)
}


#' Indexing function for continents used in data.
#' @name continent_index_fun
#' @param my_data The data with a continent column you want to index.
#' @param  my_countries A vector of continent names used in the data
#' @return returns table with continents indexed
#' @noRd

continent_index_fun <- function(my_data) {
  n_con <- as.character(unique(my_data$continent))
  my_data$index_continent <- NA
  for (i in 1:length(n_con)) {
    for (j in 1:nrow(my_data)) {
      con_name <- n_con[i]
      if (my_data$continent[j] == con_name) {
        my_data$index_continent[j] <- i
      } else {
        next
      }
    }
  }
  return(my_data)
}

#' Drop NA values
#' @name drop_na
#' @param x A vector of observations with some NA values
#' @return A cleaned vector without the NA observations
#' @source With thanks, taken from https://github.com/AlkemaLab/fpemlocal/blob/master/R/format_check.R
#' @noRd

drop_na <- function(x) {
  index <- is.na(x)
  x <- x[!index]
  return(x)
}

#' Flatten correlation matrix
#' @name flat_cor_mat
#' @param cor_r Correlation matrix
#' @return A table with 3 columns containing :
# Column 1 : row names (variable 1 for the correlation test)
# Column 2 : column names (variable 2 for the correlation test)
# Column 3 : the correlation coefficients
#' @noRd

flat_cor_mat <- function(cor_r){
  cor_r <- tibble::rownames_to_column(as.data.frame(cor_r), var = "row")
  cor_r <- tidyr::gather(cor_r, column, cor, -1)
  cor_r <- cor_r %>% dplyr::distinct(cor, .keep_all = TRUE)
  cor_r$cor <- round(cor_r$cor,1)
  cor_r <- cor_r %>% dplyr::filter(cor!=1)
  return(cor_r)
}

#' Get the DHS data used for modelling the proportion of modern contraceptives supplied by the public and private sectors at the national level.
#' @name get_national_data
#' @param local TRUE/FALSE. Default is FALSE for global runs. Decides if this is a single-country or global run.
#' @param mycountry Default is NULL. The name of country of interest. For the names of potential countries, review vignette.
#' @param fp2030 TRUE/FALSE. Default is TRUE. Filters the data to only include FP2030 countries.
#' @param surveydata_filepath Path to survey data. Default is NULL. Survey data should be a .xlsx with the following format \code{\link{national_FPsource_data}}.
#' @param trunc Default is FALSE. This argument indicates to function to use a
#' small 4 country dataset for illustration purposes.
#' See ?mcmsupply::trunc_subnat_FPsource_data or
#' ??mcmsupply::trunc_national_FPsource_data for details.
#' @return returns the DHS data set used for inputs into the model
#' @noRd

get_national_data <- function(local=FALSE, mycountry=NULL, fp2030=TRUE, surveydata_filepath=NULL, trunc=FALSE) {
  if(is.null(surveydata_filepath)==TRUE){
    if(trunc==FALSE) {
      message("Using preloaded dataset!")
      national_FPsource_data <- mcmsupply::national_FPsource_data # Read in all the data
    } else {
      message("Using preloaded truncated dataset!")
      national_FPsource_data <- mcmsupply::trunc_national_FPsource_data # Read in truncated data
    }
  } else {
    message(paste0("Using file from ", surveydata_filepath))
    national_FPsource_data <- readxl::read_xlsx(surveydata_filepath) # read in custom data
    national_FPsource_format <- mcmsupply::national_FPsource_format # Load format checker
    check_format(national_FPsource_format, national_FPsource_data) # Check if user input data is suitable for inclusion
  }

  if(fp2030==TRUE) {
    FP_2030_countries <- c("Afghanistan","Benin","Burkina Faso","Cameroon",
                           "Congo", "Congo Democratic Republic", "Cote d'Ivoire",
                           "Ethiopia", "Ghana","Guinea","India","Kenya", "Liberia", "Madagascar",
                           "Malawi","Mali", "Mozambique", "Myanmar", "Nepal", "Niger", "Nigeria", "Pakistan",
                           "Philippines", "Rwanda", "Senegal", "Sierra Leone", "Togo", "Tanzania", "Uganda", "Zimbabwe")

    national_FPsource_data <- national_FPsource_data %>% dplyr::filter(Country %in% FP_2030_countries)
  }

  FP_source_data_wide <- national_FPsource_data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Country, Super_region, Method, average_year, sector_category) %>%
    dplyr::distinct() %>%
    dplyr::select(Country, Super_region, Method, average_year, sector_category, proportion, n) %>%
    tidyr::pivot_wider(names_from = sector_category, values_from = c(proportion,n)) %>%
    dplyr::rename(Commercial_medical = proportion_Commercial_medical ) %>%
    dplyr::rename(Public = proportion_Public ) %>%
    dplyr::rename(Other = proportion_Other) %>%
    dplyr::arrange(Country)

  FP_source_data_wide <- FP_source_data_wide %>%
    dplyr::rowwise() %>%
    dplyr::mutate(check_total = sum(Commercial_medical, Other, Public, na.rm = TRUE))

  # # Fill in single missing NA values with 1-sum(others) -------------------------
  FP_source_data_wide$count_NA <- rowSums(is.na(FP_source_data_wide[, c("Commercial_medical", "Other","Public")])) # count NAs
  FP_source_data_wide <- FP_source_data_wide %>%
    dplyr::filter(count_NA <2) # Remove obs with two missing sectors
  FP_source_data_wide$remainder <- 1 - rowSums(FP_source_data_wide[, c("Commercial_medical", "Other","Public")], na.rm = TRUE)

  for(i in 1:nrow(FP_source_data_wide)) {
    if(FP_source_data_wide$count_NA[i]==1) {
      na_col_num <- which(is.na(FP_source_data_wide[i,c("Commercial_medical", "Other","Public")])) # column number of NA
      num <- which(colnames(FP_source_data_wide)=="Commercial_medical")
      FP_source_data_wide[i,na_col_num+(num-1)] <- FP_source_data_wide[i,"remainder"] # replace with remainder
    } else {
      next
    }
  }

  # Sanity check: replace any negative numbers with approximately 0
  FP_source_data_wide <- FP_source_data_wide %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Public = ifelse(Public < 0, 0.001, Public)) %>%
    dplyr::mutate(Commercial_medical = ifelse(Commercial_medical < 0, 0.001, Commercial_medical)) %>%
    dplyr::mutate(Other = ifelse(Other < 0, 0.001, Other))

  ## Remove SE missing for two sectors ---------------------
  SE_source_data_wide <- national_FPsource_data %>% # SE data
    dplyr::ungroup() %>%
    dplyr::select(Country, Super_region, Method, average_year, sector_category, SE.proportion) %>%
    tidyr::pivot_wider(names_from = sector_category, values_from = SE.proportion) %>%
    dplyr::arrange(Country) %>%
    dplyr::rename(Public.SE = Public, Commercial_medical.SE = Commercial_medical, Other.SE = Other)

  # Readjust any SE <1% to 1%
  SE_source_data_wide <- SE_source_data_wide %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Public.SE = ifelse(Public.SE < 0.01, 0.01, Public.SE)) %>%
    dplyr::mutate(Commercial_medical.SE = ifelse(Commercial_medical.SE < 0.01, 0.01, Commercial_medical.SE)) %>%
    dplyr::mutate(Other.SE = ifelse(Other.SE < 0.01, 0.01, Other.SE))

  # Count NAs to drop observations with too much missing data
  SE_source_data_wide$count_NA <- rowSums(is.na(SE_source_data_wide)) # count NAs
  SE_source_data_wide <- SE_source_data_wide %>%
    dplyr::filter(count_NA <2) # Remove obs with two missing sectors

  # Max of column SE in country for missing values
  SE_source_data_wide <- SE_source_data_wide %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Country) %>%
    dplyr::mutate(max.Other.SE = max(Other.SE, na.rm=TRUE)) %>%
    dplyr::mutate(Other.SE = ifelse(is.na(Other.SE)==TRUE, max.Other.SE, Other.SE)) %>%
    dplyr::mutate(max.Public.SE = max(Public.SE, na.rm=TRUE)) %>%
    dplyr::mutate(Public.SE = ifelse(is.na(Public.SE)==TRUE, max.Public.SE, Public.SE)) %>%
    dplyr::mutate(max.Commercial_medical.SE = max(Commercial_medical.SE, na.rm=TRUE)) %>%
    dplyr::mutate(Commercial_medical.SE = ifelse(is.na(Commercial_medical.SE)==TRUE, max.Commercial_medical.SE, Commercial_medical.SE)) %>%
    dplyr::select(Country, Super_region, Method, average_year, Commercial_medical.SE, Public.SE, Other.SE, count_NA)

  # Merge SE and proportion data together
  FP_source_data_wide <- dplyr::left_join(FP_source_data_wide, SE_source_data_wide) %>%
    dplyr::arrange(Country, Super_region, Method, average_year)

  if(local==TRUE & is.null(mycountry)==FALSE) { # Subset data for country of interest ---------------------------
    message(paste0("Getting data for ",mycountry))
    FP_source_data_wide <- FP_source_data_wide %>% dplyr::filter(Country==mycountry)
  }

  return(FP_source_data_wide)
}

#' Combines the data sources to create one JAGS input list
#' @name get_national_JAGSinput_list
#' @param pkg_data The data list from the 'mcmsupply::get_national_modelinputs' function.
#' @param local TRUE/FALSE. Default is FALSE for global runs. Decides if this is a single-country or global run.
#' @param mycountry The name of country of interest. Default is NULL. For the names of potential countries, review vigentte.
#' @return returns a list ready for input into the JAGS model
#' @noRd

get_national_JAGSinput_list <- function(pkg_data, local= FALSE,  mycountry=NULL) {
  if(local==TRUE & is.null(mycountry)==FALSE) {
    local_parms <- get_national_local_parameters(mycountry=mycountry) # Get parameters for local informative priors for national data
    jags_data <- list(y = pkg_data$data[,c("Public", "Commercial_medical")], # Combine all data into one list ready for JAGS
                      se_prop = pkg_data$data[,c("Public.SE", "Commercial_medical.SE")],
                      alphahat_region = local_parms$alphahat_region,
                      tau_alphahat_cms = local_parms$tau_alphahat_cms,
                      natRmat = local_parms$natRmat, # dwish on inverse
                      natdf = length(pkg_data$n_method)+3,
                      kstar = pkg_data$kstar,
                      B.ik = pkg_data$B.ik,
                      n_years = pkg_data$n_years,
                      n_obs = pkg_data$n_obs,
                      K = pkg_data$K,
                      H = pkg_data$H,
                      M_count = pkg_data$M_count,
                      matchmethod = pkg_data$matchmethod,
                      matchyears = pkg_data$matchyears
    )
  } else {
    estimated_rho_matrix <- mcmsupply::estimated_national_correlations %>% # Get global correlations for national data
      dplyr::select(row, column, public_cor, private_cor)
    my_SE_rho_matrix <- estimated_rho_matrix %>%
      dplyr::select(public_cor, private_cor)
    jags_data <- list(y = pkg_data$data[,c("Public", "Commercial_medical")], # Combine all data into one list ready for JAGS
                      se_prop = pkg_data$data[,c("Public.SE", "Commercial_medical.SE")],
                      rho = my_SE_rho_matrix,
                      kstar = pkg_data$kstar,
                      B.ik = pkg_data$B.ik,
                      n_years = pkg_data$n_years,
                      n_obs = pkg_data$n_obs,
                      K = pkg_data$K,
                      H = pkg_data$H,
                      C_count = pkg_data$C_count,
                      R_count = pkg_data$R_count,
                      M_count = pkg_data$M_count,
                      matchcountry = pkg_data$matchcountry,
                      matchregion = pkg_data$matchsuperregion,
                      matchmethod = pkg_data$matchmethod,
                      matchyears = pkg_data$matchyears
    )
  }
  return(jags_data)
}

#' Get the country-specific median and precision terms for the alpha intercept in local national-level model runs.
#' @name get_national_local_parameters
#' @param mycountry The name of country of interest. Default is NULL. For the names of potential countries, review vigentte.
#' @param fp2030 Default to be TRUE. The country of interest is named as one of the Family Planning 2030 focus countries discussed in the Comiskey et al. paper.
#' @return A list of local parameters to be used to inform the intercept parameter alpha and the global variance-covariance matrix used in the wishart prior of the local_model_run.txt file.
#' @noRd

get_national_local_parameters <- function(mycountry=NULL, fp2030=TRUE) {

  # Read in national alpha estimates ---------------------------------
  median_alpha_region_intercepts <- mcmsupply::median_alpha_region_intercepts # read in regional-level (alpha_rms) median estimates
  precision_alpha_country_intercepts <- mcmsupply::precision_alpha_country_intercepts # read in country-level (alpha_cms) precision estimates
  Bspline_sigma_matrix_median <- mcmsupply::Bspline_sigma_matrix_median # read in national level correlations
  mydata <- get_national_data(fp2030=fp2030) # Read complete data set in without filtering for any country

  # Match regional intercepts to country names  ------------------------
  region_country_match <- mydata %>%
    dplyr::select(Country, Super_region) %>%
    dplyr::distinct() %>%
    dplyr::filter(Country==mycountry) # List of matching countries to super-regions

  mydata <- superregion_index_fun(mydata, unique(mydata$Super_region))
  region_index_table <- tibble::tibble(Super_region = unique(mydata$Super_region), index_superregion = unique(mydata$index_superregion))
  dimnames(median_alpha_region_intercepts)[[3]] <- as.list(unlist(region_index_table$Super_region)) # Apply region names to parameter estimates
  myalpha_med <- median_alpha_region_intercepts[,,region_country_match$Super_region] # Take out relevant region

  return(list(alphahat_region = myalpha_med,
              tau_alphahat_cms = precision_alpha_country_intercepts,
              natRmat = Bspline_sigma_matrix_median))
}

#' Get JAGS model inputs
#' @name get_national_modelinputs
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @param startyear The year you wish to begin your predictions from. Default is 1990.
#' @param endyear The year you wish to finish your predictions. Default is 2030.5.
#' @param nsegments The number of knots you wish to use in your basis functions. Default is 12.
#' @param raw_data The national family planning source data from the 'get_subnational_data' function.
#' @return A list of modelling inputs for the JAGS model.
#' 1. Tstar is the year index for the most recent survey in each province.
#' 2. Kstar is the knot index that aligns with Tstar.
#' 3. B.ik are the basis functions.
#' 4. n_years are total number of years
#' 5. n_obs  are the total number of observations
#' 6. K are the number of knots.
#' 7. H is K-1. Used in the calculation of first order differences of spline coefficients.
#' 8. M_count is the number of modern contraceptive methods.
#' 9. matchcountry is the country indexing to match the observed data to the predictions.
#' 10. matchmethod is the method indexing to match the observed data to the predictions.
#' 11. matchyears is the year indexing to match the observed data to the predictions.
#' @noRd

get_national_modelinputs <- function(local=FALSE, mycountry=NULL, startyear=1990, endyear=2030.5, nsegments=12, raw_data) {

  clean_FPsource <- standard_method_names(raw_data) # Standardizing method names

  n_method <- c("Female Sterilization", "Implants", "Injectables", "IUD", "OC Pills" ) # As per the method correlation matrix
  n_country <- unique(clean_FPsource$Country)
  n_superregion <- unique(clean_FPsource$Super_region)
  clean_FPsource <- country_index_fun(clean_FPsource, n_country)
  clean_FPsource <- method_index_fun(clean_FPsource, n_method)
  clean_FPsource <- superregion_index_fun(clean_FPsource, n_superregion)

  all_years <- seq(from = startyear, to = endyear, by=0.5) # 6-monthly increments
  n_all_years <- length(all_years)

  clean_FPsource <- clean_FPsource %>%
    dplyr::mutate(index_year = match(average_year,all_years)) # Time indexing - important for splines

  T_star <- clean_FPsource %>%
    dplyr::group_by(Country) %>%
    dplyr::filter(index_year==max(index_year)) %>%
    dplyr::select(Country, index_country, average_year, index_year) %>%
    dplyr::arrange(index_country) %>%
    dplyr::ungroup() %>%
    dplyr::select(index_country, index_year, average_year) %>%
    dplyr::distinct()

  nseg=nsegments   # Default is 12
  Kstar <- vector()
  B.ik <- array(dim = c(length(n_country), length(all_years),nseg+3))
  knots.all <- matrix(nrow = length(n_country), ncol=nseg+3)
  for(i in 1:nrow(T_star)) {
    index_mc <- T_star$average_year[i]
    res <- bs_bbase_precise(all_years, lastobs=index_mc, nseg = nseg)
    B.ik[i,,] <- res$B.ik
    Kstar[i] <- res$Kstar
    knots.all[i,] <- res$knots.k
  }

  K <- dim(res$B.ik)[2]
  H <- K-1
  if(local==TRUE) {
    B.ik <- B.ik[1,,] # change array to matrix for one country
  }

  t_seq_2 <- floor(clean_FPsource$index_year) # Time sequence for countries
  country_seq <- clean_FPsource$Country
  n_country <- as.character(unique(country_seq))
  n_sector <- c("Public", "Commercial_medical", "Other") # Names of categories
  n_obs <- nrow(clean_FPsource) # Total number of observations
  year_seq <- seq(min(t_seq_2),max(t_seq_2), by=1)
  n_years <- length(year_seq) # number of years

  # Find the indexes of observations that match to the predicted responses
  match_country <- clean_FPsource$index_country
  match_years <- clean_FPsource$index_year
  match_method <- clean_FPsource$index_method
  region_country <- clean_FPsource %>%
    dplyr::select(Country, Super_region, index_country, index_superregion) %>%
    dplyr::distinct()
  match_superregion <- region_country$index_superregion

  return(list(data = clean_FPsource,
              tstar = T_star$index_year,
              kstar = Kstar,
              B.ik = B.ik,
              n_years = n_all_years,
              n_obs = n_obs,
              K = K,
              H = H,
              C_count = length(n_country),
              M_count = length(n_method),
              R_count = length(n_superregion),
              n_method = n_method, # As per the method correlation matrix
              n_country = n_country,
              n_super_region = n_superregion,
              all_years = all_years,
              matchsuperregion = match_superregion,
              matchcountry = match_country,
              matchmethod = match_method,
              matchyears = match_years)
  )
}

#' Get median and 95% credible interval for posterior samples of P from national JAGS model
#' @name get_national_P_median_quantiles
#' @param country_index_table Dataframe with country indexing applied. Used to match estimates to data.
#' @param method_index_table Dataframe with method indexing applied. Used to match estimates to data.
#' @param sector_index_table Dataframe with sector indexing applied. Used to match estimates to data.
#' @param year_index_table Dataframe with time indexing applied. Used to match estimates to data.
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all countries. local=TRUE retrieves data for only one country.
#' @param my_model JAGS model
#' @return Dataframe of labelled posterior samples with median and 95% credible intervals estimates.
#' @noRd

get_national_P_median_quantiles <- function(country_index_table, method_index_table, sector_index_table, year_index_table, my_model, local=FALSE) { # Median alpha values
  years <- unique(year_index_table$floored_year)
  n_years <- years %>% length() # important when using 6-monthly description
  n_all_years <- nrow(year_index_table)
  if(local==FALSE) {
    P_samp <- my_model$BUGSoutput$sims.list$P
    P_dims <- dim(P_samp)
    print(P_dims)

    P_s_med <- array(dim=c(P_dims[4],n_years,P_dims[3],P_dims[2])) # 30x5 matrix for CountryxMethod then 5 into an array for each sector

    # Create a table for storing individual true country public data
    for(k in 1:n_years) { # time loop
      year <- years[k]
      time_index <- year_index_table %>% # match index years to pooled years (pool 6 monthly estimates)
        dplyr::filter(floored_year == year) %>%
        dplyr::select(index_year) %>%
        unlist() %>%
        as.vector()
      for(j in 1:P_dims[4]) { # country
        for (r in 1:P_dims[3]) { # method
          for (i in 1:P_dims[2]) { # sector
            P_s_med[j,k,r,i] <- stats::median(P_samp[,i,r,j,time_index])
          }
        }
      }
    }

    P_s_med <- plyr::adply(P_s_med, c(1,2,3,4))
    colnames(P_s_med) <- c("index_country", "index_year", "index_method", "index_sector", "median_p")

    P_s_med <- P_s_med %>%
      dplyr::mutate(
      index_country = as.numeric(index_country),
      index_method = as.numeric(index_method),
      index_sector = as.numeric(index_sector),
      index_year = as.numeric(index_year)
    )

    upper80_P_s_med <- lower80_P_s_med <- upper95_P_s_med <- lower95_P_s_med <- array(dim=c(P_dims[4],n_years,P_dims[3],P_dims[2])) # 30x5 matrix for CountryxMethod then 5 into an array for each sector

    # Create a table for storing individual true country public data
    for(t in 1:n_years) { # time loop
      year <- years[t]
      time_index <- year_index_table %>% # match index years to pooled years (pool 6 monthly estimates)
        dplyr::filter(floored_year == year) %>%
        dplyr::select(index_year) %>%
        unlist() %>%
        as.vector()

      for(j in 1:P_dims[4]) {
        for (r in 1:P_dims[3]) { # Create a table for storing individual true country public data
          for (i in 1:P_dims[2]) {
            upper95_P_s_med[j,t,r,i] <- stats::quantile(P_samp[,i,r,j,time_index], probs = 0.975, na.rm=TRUE) # upper quantile using the whole year
            upper80_P_s_med[j,t,r,i] <- stats::quantile(P_samp[,i,r,j,time_index], probs = 0.9, na.rm=TRUE) # upper quantile using the whole year
          }
        }
      }
    }

    upper95_P_s_med <- plyr::adply(upper95_P_s_med, c(1,2,3,4))
    colnames(upper95_P_s_med) <- c("index_country", "index_year", "index_method", "index_sector", "upper_95")
    upper80_P_s_med <- plyr::adply(upper80_P_s_med, c(1,2,3,4))
    colnames(upper80_P_s_med) <- c("index_country", "index_year", "index_method", "index_sector", "upper_80")

    for(t in 1:n_years) { # time loop
      year <- years[t]
      time_index <- year_index_table %>% # match index years to pooled years (pool 6 monthly estimates)
        dplyr::filter(floored_year == year) %>%
        dplyr::select(index_year) %>%
        unlist() %>%
        as.vector()
      for(j in 1:P_dims[4]) { # country loop
        for (r in 1:P_dims[3]) { # method loop
          for (i in 1:P_dims[2]) { # sector loop
            lower95_P_s_med[j,t,r,i] <- stats::quantile(P_samp[,i,r,j,time_index], probs = 0.025) # lower quantile using the whole year
            lower80_P_s_med[j,t,r,i] <- stats::quantile(P_samp[,i,r,j,time_index], probs = 0.1) # lower quantile using the whole year
          }
        }
      }
    }

    lower95_P_s_med <- plyr::adply(lower95_P_s_med, c(1,2,3,4)) # changes shape of array to matrix
    colnames(lower95_P_s_med) <- c("index_country", "index_year", "index_method", "index_sector", "lower_95")
    lower80_P_s_med <- plyr::adply(lower80_P_s_med, c(1,2,3,4)) # changes shape of array to matrix
    colnames(lower80_P_s_med) <- c("index_country", "index_year", "index_method", "index_sector", "lower_80")

    P_s_Q <- merge(lower80_P_s_med, upper80_P_s_med)
    P_s_Q <- merge(P_s_Q,lower95_P_s_med)
    P_s_Q <- merge(P_s_Q,upper95_P_s_med)

    P_s_Q <- P_s_Q %>%
      dplyr::mutate(
        index_country = as.numeric(index_country),
        index_method = as.numeric(index_method),
        index_sector = as.numeric(index_sector),
        index_year = as.numeric(index_year)
      )


    P_s_med <- P_s_med %>%
      dplyr::left_join(P_s_Q)

    # Match mid-year indexing to model posterior samples
    whole_years <- year_index_table %>%
      dplyr::select(floored_year) %>%
      unlist() %>%
      as.vector() %>%
      unique()
    time_tb <- tibble::tibble(index_year = 1: length(whole_years), average_year = whole_years + 0.5)

    P_s_med <- P_s_med %>%
      dplyr::left_join(country_index_table) %>% # match strings to numeric indexing
      dplyr::left_join(time_tb) %>%
      dplyr::left_join(method_index_table) %>%
      dplyr::left_join(sector_index_table) %>%
      dplyr::group_by(Country, Method, Sector, average_year) %>%
      dplyr::select(Country, Method, Sector, average_year, median_p, lower_80, upper_80, lower_95, upper_95) %>%
      dplyr::distinct() %>%
      dplyr::rowwise() %>%
      dplyr::filter(average_year > floor(average_year)) # only take the mid-years

  } else {
    mycountry <- unique(country_index_table$Country)
    m <- my_model$BUGSoutput$sims.matrix # create an object containing the posterior samples
    sample_draws <- tidybayes::tidy_draws(m)     ## format data for plotting results
    n_iter <- nrow(sample_draws)
    P_start <- "P[1,1,1]"
    P_end <- paste0("P[",nrow(sector_index_table),",",nrow(method_index_table),",",nrow(year_index_table),"]")
    col1 <- which(colnames(sample_draws)==P_start)
    col2 <- which(colnames(sample_draws)==P_end)
    tnp_samp <- sample_draws[,c(col1:col2)]
    P_s_med <- tnp_samp %>%
      tidyr::pivot_longer(cols = dplyr::everything(),
                          names_to = "params",
                          values_to = "mu_pred"
      ) %>%
      dplyr::mutate(index_sector = as.numeric(substr(params, nrow(sector_index_table),nrow(sector_index_table)))) %>%
      dplyr::mutate(index_method = as.numeric(substr(params, nrow(method_index_table),nrow(method_index_table)))) %>%
      dplyr::mutate(index_year = rep(rep(1:n_all_years, each = nrow(sector_index_table)*nrow(method_index_table)), n_iter)) %>%
      dplyr::left_join(year_index_table) %>%
      dplyr::left_join(method_index_table) %>%
      dplyr::left_join(sector_index_table) %>%
      dplyr::select(mu_pred, average_year, Method, Sector) %>%
      dplyr::group_by(Method, Sector, average_year) %>%
      dplyr::mutate(median_p = stats::median(mu_pred),
                    lower_80 = stats::quantile(mu_pred, prob = 0.01),
                    upper_80 = stats::quantile(mu_pred, prob = 0.9),
                    lower_95 = stats::quantile(mu_pred, prob = 0.025),
                    upper_95 = stats::quantile(mu_pred, prob = 0.975))%>%
      dplyr::mutate(Country = mycountry) %>%
      dplyr::select(Country, Method, Sector, average_year, median_p, lower_80, upper_80, lower_95, upper_95) %>%
      dplyr::distinct()
  }
  return(P_s_med)
}

#' Get the r and z ratio point estimates from the separate chains of the JAGS model runs
#' R and Z are the intermediate parameters that are used to estimates the final proportions. See the model file for context.
#' @name get_national_P_point_estimates
#' @param pkg_data Output of the `mcmsupply::get_subnational_modelinputs()` function.
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @return returns the point estimates for the jags model object
#' @noRd

get_national_P_point_estimates <- function(pkg_data, local=FALSE, mycountry=NULL) {

  if(local==FALSE) {
    f <- file.path(tempdir(), "mod_global_national_results.RDS")
    mymod <- readRDS(f)
    } else {
      f <- file.path(tempdir(), paste0("mod_",mycountry,"_national_results.RDS"))
      mymod <- readRDS(f)
    }

  # Get model inputs for cross matching to estimates
  n_country <- pkg_data$n_country
  n_method <- pkg_data$n_method
  n_sector <- c("Public", "Commercial_medical", "Other")
  n_all_years <- pkg_data$n_years
  mydata <- pkg_data$data
  all_years <- pkg_data$all_years
  K <- pkg_data$K
  B.ik <- pkg_data$B.ik

  # Creating index tables for reference ------------------------------------------------------------
  country_index_table <- tibble::tibble(Country = n_country, index_country = unique(mydata$index_country))
  method_index_table <- tibble::tibble(Method = n_method, index_method = 1:5)
  sector_index_table <- tibble::tibble(Sector = n_sector, index_sector = 1:3)
  year_index_table <- tibble::tibble(average_year = all_years,
                                     index_year = 1:n_all_years,
                                     floored_year = floor(all_years))

  P_point_estimates <- get_national_P_median_quantiles(country_index_table, method_index_table, sector_index_table, year_index_table, my_model= mymod, local=local)

  if(is.null(mycountry)==TRUE) {
    f <- file.path(tempdir(), "P_point_national_estimates.RDS")
    saveRDS(P_point_estimates, f)
  } else {
    f <- file.path(tempdir(), paste0(mycountry,"_P_point_national_estimates.RDS"))
    saveRDS(P_point_estimates, f)
  }
  return(P_point_estimates)
}

#' Wrapper function to run the jags model for estimating the proportion of modern contraceptive methods supplied by the public & private Sectors using a Bayesian hierarchical penalized spline model for the national and subnational administration levels
#' @name get_point_estimates
#' @param jagsdata Output of the `mcmsupply::get_modelinputs()` function.
#' @param n_chain Default is 2. Number of chains to run in your MCMC sample.
#' @param ... Arguments from the `mcmsupply::get_modelinputs()` function.
#' @return returns the point estimates (median, 80% and 95% credible intervals) from the JAGS output
#' @noRd

get_point_estimates <- function(jagsdata, n_chain, ...) {
  args <- jagsdata$args
  national <- args$national
  if(national==TRUE) {
   p <- get_national_P_point_estimates(pkg_data = jagsdata$modelinputs, local=args$local, mycountry=args$mycountry)
  } else {
   p <- get_subnational_P_point_estimates(pkg_data = jagsdata$modelinputs, local=args$local, mycountry=args$mycountry, n_chain=n_chain)
  }
  message("Results complete!")
  return(p)
}

#' Get subnational family planning source data
#' @name get_subnational_data
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @param fp2030 Default is TRUE. Filter raw data to only include the Family Planning 2030 focus countries discussed in the Comiskey et al. paper.
#' @param surveydata_filepath Path to survey data. Default is NULL. Survey data should be a .xlsx with the following format \code{\link{subnat_FPsource_data}}.
#' @param trunc Default is FALSE. This argument indicates to function to use a
#' small 4 country dataset for illustration purposes.
#' See ?mcmsupply::trunc_subnat_FPsource_data or
#' ??mcmsupply::trunc_national_FPsource_data for details.
#' @return The input data for your country of interest, used as an input to the mcmsupply model
#' @noRd

get_subnational_data <- function(local=FALSE, mycountry=NULL, fp2030=TRUE, surveydata_filepath=NULL, trunc=FALSE) {
  if(is.null(surveydata_filepath)==TRUE){
    if(trunc==FALSE) {
      message("Using preloaded dataset!")
      subnat_FPsource_data <- mcmsupply::subnat_FPsource_data  # Read subnational in SE data
    } else {
      message("Using preloaded truncated dataset!")
      subnat_FPsource_data <- mcmsupply::trunc_subnat_FPsource_data  # Read subnational in SE data
    }
  } else {
    subnat_FPsource_data <- readxl::read_xlsx(surveydata_filepath)
    subnat_FPsource_format <- mcmsupply::subnat_FPsource_format
    check_format(subnat_FPsource_format, subnat_FPsource_data) # Check if user input data is suitable for inclusion
  }
  subnatSE_source_data <- subnat_FPsource_data %>%
    dplyr::filter(Region!="NA")

  FP_source_data_wide <- subnat_FPsource_data %>% # Proportion data
    dplyr::ungroup() %>%
    dplyr::select(Country, Region, Method,  average_year, sector_categories, proportion, SE.proportion, n) %>%
    tidyr::pivot_wider(names_from = sector_categories, values_from = c(proportion,SE.proportion,n)) %>% # separate data into columns for each sector
    dplyr::rename(Commercial_medical = proportion_Commercial_medical,
                  Public = proportion_Public,
                  Other = proportion_Other,
                  Public.SE = SE.proportion_Public,
                  Commercial_medical.SE = SE.proportion_Commercial_medical,
                  Other.SE = SE.proportion_Other) %>%
    dplyr::arrange(Country) %>%
    dplyr::filter(n_Commercial_medical>=10 | n_Public>=10 | n_Other>=10)

  FP_source_data_wide <- FP_source_data_wide %>%
    dplyr::rowwise() %>%
    dplyr::mutate(check_total = sum(Commercial_medical, Other, Public, na.rm = TRUE)) %>% # make sure proportions add to 1
    dplyr::select(Country, Region, Method, average_year, Commercial_medical, Other, Public, Commercial_medical.SE, Other.SE, Public.SE, n_Commercial_medical, n_Other, n_Public, check_total)

  # When check_Total=1, replace missing values with 0
  for (i in 1:nrow(FP_source_data_wide)) {
    if(FP_source_data_wide$check_total[i]>0.99) {
      na_cols <- which(is.na(FP_source_data_wide[i, c("Commercial_medical", "Other", "Public")])==TRUE) # NA values
      FP_source_data_wide[i, na_cols+4] <- as.list(rep(0, length(na_cols)))
    }
  }

  # Transform exactly 1 and 0 values away from boundary using lemon-squeezer approach
  FP_source_data_wide <- FP_source_data_wide %>%
    dplyr::mutate(Commercial_medical = (Commercial_medical*(nrow(FP_source_data_wide)-1)+0.33)/nrow(FP_source_data_wide)) %>%   # Y and SE transformation to account for (0,1) limits (total in sector)
    dplyr::mutate(Other = (Other*(nrow(FP_source_data_wide)-1)+0.33)/nrow(FP_source_data_wide)) %>%
    dplyr::mutate(Public = (Public*(nrow(FP_source_data_wide)-1)+0.33)/nrow(FP_source_data_wide)) %>%
    dplyr::select(Country, Region, Method, average_year, Commercial_medical, Other, Public, Commercial_medical.SE, Other.SE, Public.SE, n_Other, n_Public, n_Commercial_medical, check_total) #, count_NA, remainder)

  # Address issues with SE values
  FP_source_data_wide$count_SE.NA <- rowSums(is.na(FP_source_data_wide %>% dplyr::select(Public.SE, Commercial_medical.SE, Other.SE))) # count NAs
  SE_source_data_wide_norm <- FP_source_data_wide %>%
    dplyr::filter(count_SE.NA==0 & Commercial_medical.SE>0 & Public.SE>0) # Normal observations: No action needed.
  SE_source_data_wide_X <- FP_source_data_wide %>%
    dplyr::filter(Commercial_medical.SE==0 | Public.SE==0) # Issue observations: Two missing sectors. Action required.

  # Replacing SE=0 and no NAs: Replacing with DHS manual estimate for SE.
  SE_source_data_wide_X <- SE_source_data_wide_X %>%
    dplyr::filter(n_Public>=20 | n_Commercial_medical >=20 | n_Other >=20) # Remove small sample sizes (DHS has 10 units sampled per cluster as min., 20 as average)

  for(i in 1:nrow(SE_source_data_wide_X)) {
    num.SE0 <- which(SE_source_data_wide_X[i,c("Commercial_medical.SE","Other.SE","Public.SE")]==0)
    numSEnon0 <- which(SE_source_data_wide_X[i,c("Commercial_medical.SE","Other.SE","Public.SE")]!=0)
    if(length(num.SE0)==1) {
      mean.SE <- mean(as.vector(unlist(SE_source_data_wide_X[i,7+numSEnon0]))) # Noticed that other two columns have identical SE. Assign SE to third column.
      SE_source_data_wide_X[i,7+num.SE0] <- mean.SE
    } else {
      N1 <- length(which(SE_source_data_wide_X$Public>0.99)) + length(which(SE_source_data_wide_X$Commercial_medical>0.99)) # Number of obs=1
      phat <- (N1 + 1/2)/(nrow(FP_source_data_wide)+1)
      SE.hat <- sqrt((phat*(1-phat))/(N1+1))
      SE_source_data_wide_X[i,7+num.SE0] <- SE.hat
    }
  }

  FP_source_data_wide <- dplyr::bind_rows(SE_source_data_wide_norm, SE_source_data_wide_X) # Put data back together again

  # Remove proportions with two sectors still missing
  mydata <- FP_source_data_wide %>%
    dplyr::filter(is.na(Public)==FALSE & is.na(Other)==FALSE | is.na(Public)==FALSE & is.na(Commercial_medical)==FALSE | is.na(Commercial_medical)==FALSE & is.na(Other)==FALSE)

  # Get single country data
  if(local==TRUE & is.null(mycountry)==FALSE &is.null(surveydata_filepath)==TRUE) {
    message(paste0("Getting data for ",mycountry))
    mydata <- mydata %>% dplyr::filter(Country==mycountry)

    mydata <- mydata %>%
      droplevels() # remove factor levels of other countries
  }

  mydata <- clean_subnat_names(fp2030=fp2030, mydata) # Clean subnational region names for Rwanda, Nigeria, Cote d'Ivoire
  mydata <- mydata %>% dplyr::arrange(Country, Region, Method, average_year)

  return(mydata)
}

#' Get posterior samples of commercial medical sector from r and z variables of JAGS model
#' @name get_subnational_global_P_CM
#' @return Saved samples for commercial medical supply shares.
#' @noRd

get_subnational_global_P_CM <- function() {
  fpub <- file.path(tempdir(), "P_Public.RDS")
  P_public <- readRDS(fpub)   ## Estimating all the Categories here (including total private)
  fr <- file.path(tempdir(), "rsamps.RDS")
  r <- readRDS(fr)
  P_CM <- (1/(1+exp(-(r))))*(1-P_public)
  fCM <- file.path(tempdir(), "P_CM.RDS")
  saveRDS(P_CM, fCM)
  return(P_CM = P_CM)
}

#' Get median, 95% and 80% credible intervals for posterior samples of P from JAGS model
#' @name get_subnational_global_P_estimates
#' @param P_samp Output of the `mcmsupply::get_subnational_global_P_samps()` function.
#' @param subnat_index_table Dataframe with subnational district indexing applied. Used to match estimates to data.
#' @param method_index_table Dataframe with method indexing applied. Used to match estimates to data.
#' @param sector_type String. Name of sector you are interested in. One of either ("Public", "Commercial medical", "Other")
#' @param year_index_table Dataframe with time indexing applied. Used to match estimates to data.
#' @return Dataframe of labelled posterior samples with median, 95% and 80% credible intervals estimates.
#' @noRd

get_subnational_global_P_estimates <- function(P_samp, subnat_index_table, method_index_table, sector_type, year_index_table) { # Median alpha values
  f <- file.path(tempdir(), P_samp)
  P_samps <- readRDS(f)  #system.file(main_path, P_samp, package = "mcmsupply")

  time_index <- year_index_table %>% # match index years to pooled years (pool 6 monthly estimates)
    dplyr::filter(average_year>floored_year) %>%
    dplyr::select(index_year) %>%
    unlist() %>%
    as.vector()

  averageyear_index_table <- year_index_table %>%
    dplyr::filter(average_year>floored_year) %>%
    dplyr::select(average_year, index_year) %>%
    dplyr::mutate(index_year = 1:dplyr::n())

  P_dims <- dim(P_samps)

  #### P median
  P_s_med <- array(dim=c(length(time_index),P_dims[2],P_dims[3])) # method, year, subnat

  # Create a table for storing individual true country public data
  for(k in 1:length(time_index)) { # time loop
    for(s in 1:P_dims[3]) { # subnat
      for (m in 1:P_dims[2]) { # method
        P_s_med[k,m,s] <- stats::median(P_samps[,m,s,time_index[k]])
      }
    }
  }

  P_s_med <- plyr::adply(P_s_med, c(1,2,3))
  colnames(P_s_med) <- c("index_year", "index_method", "index_subnat", "median_p")

  P_s_med <- P_s_med %>%
    dplyr::mutate(index_year = as.numeric(index_year)) %>%
    dplyr::mutate(index_method = as.numeric(index_method)) %>%
    dplyr::mutate(index_subnat = as.numeric(index_subnat)) %>%
    dplyr::left_join(subnat_index_table) %>%
    dplyr::left_join(method_index_table) %>%
    dplyr::left_join(averageyear_index_table) %>%
    dplyr::mutate(Sector = sector_type)

  #### P median
  P_s_quant <- array(dim=c(length(time_index),P_dims[2],P_dims[3],4)) # method, year, subnat, quantile(95, 80)

  # Create a table for storing individual true country public data
  for(k in 1:length(time_index)) { # time loop
    for(s in 1:P_dims[3]) { # subnat
      for (m in 1:P_dims[2]) { # method
        P_s_quant[k,m,s,1:2] <- as.vector(unlist(stats::quantile(P_samps[,m,s,time_index[k]], prob=c(0.025, 0.975))))
        P_s_quant[k,m,s,3:4] <- as.vector(unlist(stats::quantile(P_samps[,m,s,time_index[k]], prob=c(0.1, 0.9))))
      }
    }
  }

  P_s_quant <- plyr::adply(P_s_quant, c(1,2,3))
  colnames(P_s_quant) <- c("index_year", "index_method", "index_subnat", "lower_95", "upper_95", "lower_80", "upper_80")

  P_s_quant <- P_s_quant %>%
    dplyr::mutate(index_year = as.numeric(index_year)) %>%
    dplyr::mutate(index_method = as.numeric(index_method)) %>%
    dplyr::mutate(index_subnat = as.numeric(index_subnat)) %>%
    dplyr::left_join(subnat_index_table) %>%
    dplyr::left_join(method_index_table) %>%
    dplyr::left_join(averageyear_index_table) %>%
    dplyr::mutate(Sector = sector_type)

  P_med_quantile <- dplyr::left_join(P_s_med, P_s_quant)
  return(P_med_quantile)
}

#' Get posterior samples of other sector from r and z variables of JAGS model
#' @name get_subnational_global_P_other
#' @return Saved samples for other supply shares.
#' @noRd

get_subnational_global_P_other <- function() {
  fpub <- file.path(tempdir(), "P_Public.RDS") # create pathway
  P_public <- readRDS(fpub)
  fCM <- file.path(tempdir(), "P_CM.RDS")
  P_CM <- readRDS(fCM)
  P_other <- (1-P_public) - P_CM
  fOther<- file.path(tempdir(), "P_Other.RDS")
  saveRDS(P_other, fOther)
  return(P_other = P_other)
}

#' Get posterior samples of public sector from r and z variables of JAGS model
#' @name get_subnational_global_P_public
#' @return Saved samples for public supply shares.
#' @noRd

get_subnational_global_P_public <- function() {
  fz <- file.path(tempdir(), "zsamps.RDS")
  z <- readRDS(fz)
  P_public <- 1/(1+exp(-(z)))
  fpub <- file.path(tempdir(), "P_Public.RDS") # create pathway
  saveRDS(P_public, fpub)   ## Estimating all the Categories here (including total private)
  return(P_public = P_public)
}

#' Get posterior samples of P from r and z variables of JAGS model
#' @name get_subnational_global_P_samps
#' @return Saved samples for public, commercial medical and other supply shares.
#' @noRd

get_subnational_global_P_samps <- function() {

  P_public <- get_subnational_global_P_public()
  gc()
  P_CM <- get_subnational_global_P_CM()
  gc()
  P_other <- get_subnational_global_P_other()
  gc()
  return(list(P_public = P_public,
              P_CM = P_CM,
              P_other = P_other))
}

#' Combines the data sources to create one JAGS input list
#' @name get_subnational_JAGSinput_list
#' @param pkg_data The data list from the 'mcmsupply::get_national_modelinputs' function.
#' @param local TRUE/FALSE. Default is FALSE for global runs. Decides if this is a single-country or global run.
#' @param mycountry The name of country of interest. Default is NULL. For the names of potential countries, review vignette.
#' @return returns a list ready for input into the JAGS model
#' @noRd

get_subnational_JAGSinput_list <- function(pkg_data, local= FALSE, mycountry=NULL) {
  if(local==TRUE & is.null(mycountry)==FALSE) {
    local_params <- get_subnational_local_parameters(mycountry = mycountry) # local informative prior parameters
    jags_data <- list(y = pkg_data$data[,c("Public", "Commercial_medical")], # create JAGS list
                      se_prop = pkg_data$data[,c("Public.SE", "Commercial_medical.SE")],
                      alpha_cms_hat = local_params$alpha_cms,
                      tau_alpha_pms_hat = local_params$tau_alphapms,
                      inv.sigma_delta = local_params$inv.sigma_delta,
                      kstar = pkg_data$kstar,
                      B.ik = pkg_data$B.ik,
                      n_years = pkg_data$n_years,
                      n_obs = pkg_data$n_obs,
                      K = pkg_data$K,
                      H = pkg_data$H,
                      P_count = pkg_data$P_count,
                      M_count = pkg_data$M_count,
                      matchsubnat = pkg_data$matchsubnat,
                      matchmethod = pkg_data$matchmethod,
                      matchyears = pkg_data$matchyears
    )
  } else {
    estimated_global_subnational_correlations <- mcmsupply::estimated_global_subnational_correlations  # load global subnational correlations
    estimated_rho_matrix <- estimated_global_subnational_correlations %>%
      dplyr::select(row, column, public_cor, private_cor)
    my_SE_rho_matrix <- estimated_rho_matrix %>%
      dplyr::select(public_cor, private_cor)
    jags_data <- list(y = pkg_data$data[,c("Public", "Commercial_medical")], # create JAGS list
                      se_prop = pkg_data$data[,c("Public.SE", "Commercial_medical.SE")],
                      rho = my_SE_rho_matrix,
                      kstar = pkg_data$kstar,
                      B.ik = pkg_data$B.ik,
                      n_years = pkg_data$n_years,
                      n_obs = pkg_data$n_obs,
                      K = pkg_data$K,
                      H = pkg_data$H,
                      C_count = pkg_data$C_count,
                      P_count = pkg_data$P_count,
                      R_count = pkg_data$R_count,
                      M_count = pkg_data$M_count,
                      matchsubnat = pkg_data$matchsubnat,
                      matchcountry = pkg_data$matchcountry,
                      matchregion = pkg_data$matchsuperregion,
                      matchmethod = pkg_data$matchmethod,
                      matchyears = pkg_data$matchyears)
  }
  return(jags_data)
}

#' Get median, 95% and 80% credible intervals for posterior samples of P from local JAGS model
#' @name get_subnational_local_P_estimates
#' @param Psamps posterior samples of P for one sector from JAGS model
#' @param param_names names of the parameters you wish to summarise
#' @param subnat_index_table Dataframe with subnational district indexing applied. Used to match estimates to data.
#' @param method_index_table Dataframe with method indexing applied. Used to match estimates to data.
#' @param sector_index_table Dataframe with sector indexing applied. Used to match estimates to data.
#' @param year_index_table Dataframe with time indexing applied. Used to match estimates to data.
#' @return Dataframe of labelled posterior samples with median, 95% and 80% credible intervals estimates.
#' @noRd

get_subnational_local_P_estimates <- function(Psamps, param_names, subnat_index_table, method_index_table, sector_index_table, year_index_table) {
  samp_med <- Psamps %>%
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ stats::median(.x, na.rm = TRUE))) %>%
    dplyr::select(tidyr::all_of(param_names)) %>%
    tidyr::pivot_longer(tidyr::all_of(param_names),
                        names_to = "params",
                        values_to = "median_p") %>%
    tidyr::separate(params, c("P" ,"index_sector", "index_method", "index_subnat", "index_year")) %>%
    dplyr::select(!P)

  samp_lwr95 <- Psamps %>%
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ stats::quantile(.x, prob = 0.025, na.rm = TRUE))) %>%
    dplyr::select(tidyr::all_of(param_names)) %>%
    tidyr::pivot_longer(tidyr::all_of(param_names),
                        names_to = "params",
                        values_to = "lower_95") %>%
    tidyr::separate(params, c("P" ,"index_sector", "index_method", "index_subnat", "index_year")) %>%
    dplyr::select(!P)

  samp_upr95 <- Psamps %>%
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ stats::quantile(.x, prob = 0.975, na.rm = TRUE))) %>%
    dplyr::select(tidyr::all_of(param_names)) %>%
    tidyr::pivot_longer(tidyr::all_of(param_names),
                        names_to = "params",
                        values_to = "upper_95") %>%
    tidyr::separate(params, c("P" ,"index_sector", "index_method", "index_subnat", "index_year")) %>%
    dplyr::select(!P)

  samp_lwr80 <- Psamps %>%
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ stats::quantile(.x, prob = 0.1, na.rm = TRUE))) %>%
    dplyr::select(tidyr::all_of(param_names)) %>%
    tidyr::pivot_longer(tidyr::all_of(param_names),
                        names_to = "params",
                        values_to = "lower_80") %>%
    tidyr::separate(params, c("P" ,"index_sector", "index_method", "index_subnat", "index_year")) %>%
    dplyr::select(!P)

  samp_upr80 <- Psamps %>%
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ stats::quantile(.x, prob = 0.9, na.rm = TRUE))) %>%
    dplyr::select(tidyr::all_of(param_names)) %>%
    tidyr::pivot_longer(tidyr::all_of(param_names),
                        names_to = "params",
                        values_to = "upper_80") %>%
    tidyr::separate(params, c("P" ,"index_sector", "index_method", "index_subnat", "index_year")) %>%
    dplyr::select(!P)

  tmp_summary <- samp_med %>%
    dplyr::left_join(samp_lwr95) %>%
    dplyr::left_join(samp_upr95) %>%
    dplyr::left_join(samp_lwr80) %>%
    dplyr::left_join(samp_upr80) %>%
    dplyr::mutate(dplyr::across(index_sector:upper_80, as.numeric)) %>%
    dplyr::left_join(year_index_table) %>%
    dplyr::left_join(method_index_table) %>%
    dplyr::left_join(sector_index_table) %>%
    dplyr::left_join(subnat_index_table) %>%
    dplyr::group_by(Region, Method, Sector, average_year) %>%
    dplyr::select(Country, Region, Method, Sector, average_year, median_p, lower_95, upper_95, lower_80, upper_80) %>%
    dplyr::distinct() %>%
    dplyr::rowwise() %>%
    dplyr::filter(average_year > floor(average_year))

  return(tmp_summary)
}

#' Get the country-specific median and precision terms for the alpha intercept in local national-level model runs.
#' @name get_subnational_local_parameters
#' @param mycountry The name of country of interest. Default is NULL. For the names of potential countries, review vigentte.
#' @return A list of local parameters to be used to inform the intercept parameter alpha and the global variance-covariance matrix used in the wishart prior of the local_model_run.txt file.
#' @noRd

get_subnational_local_parameters <- function(mycountry) {
  inv.sigma_delta_hat <- mcmsupply::inv.sigma_delta_hat_subnationalmod # multi-country variance covariance matrix
  median_alphacms <- mcmsupply::median_alphacms # load country-level intercept median estimates
  myalpha_med <- median_alphacms[,,mycountry] # Take out relevant country
  tau_alpha_pms_hat <- mcmsupply::tau_alphapms_subnationalmod # load subnational-level intercept precision
  return(list(alpha_cms = myalpha_med,
              tau_alphapms = tau_alpha_pms_hat,
              inv.sigma_delta = inv.sigma_delta_hat))
}

#' Get JAGS model inputs
#' @name get_subnational_modelinputs
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @param startyear The year you wish to begin your predictions from. Default is 1990.
#' @param endyear The year you wish to finish your predictions. Default is 2030.5.
#' @param nsegments The number of knots you wish to use in your basis functions. Default is 12.
#' @param raw_data The subnational family planning source data from the 'get_subnational_data' function.
#' @return A list of modelling inputs for the JAGS model.
#' 1. Tstar is the year index for the most recent survey in each province.
#' 2. Kstar is the knot index that aligns with Tstar.
#' 3. B.ik are the basis functions.
#' 4. n_years are total number of years
#' 5. n_obs  are the total number of observations
#' 6. K are the number of knots.
#' 7. H is K-1. Used in the calculation of first order differences of spline coefficients.
#' 8. P_count is the number of subnational provinces/regions.
#' 9. M_count is the number of modern contraceptive methods.
#' 10. matchsubnat is the subnational province indexing to match the observed data to the predictions.
#' 11. matchcountry is the country indexing to match the observed data to the predictions.
#' 12. matchmethod is the method indexing to match the observed data to the predictions.
#' 13. matchyears is the year indexing to match the observed data to the predictions.
#' @noRd

get_subnational_modelinputs <- function(local=FALSE, mycountry=NULL, startyear=1990, endyear=2030.5, nsegments=12, raw_data) {

  if(local==TRUE & is.null(mycountry)==FALSE) {
    clean_FPsource <- raw_data %>% dplyr::filter(Country==mycountry) # Subset data to country of interest
  } else {
    clean_FPsource <- raw_data
  }

  # # Remove sample size less than 3, replace tiny SE with 1% --------------------
  clean_FPsource <- clean_FPsource %>%
    dplyr::filter(n_Other >= 3 | n_Public >= 3 | n_Commercial_medical >= 3) %>%
    dplyr::mutate(Other.SE = ifelse(Other.SE < 0.01, 0.01, Other.SE)) %>%
    dplyr::mutate(Public.SE = ifelse(Public.SE < 0.01, 0.01, Public.SE)) %>%
    dplyr::mutate(Commercial_medical.SE = ifelse(Commercial_medical.SE < 0.01, 0.01, Commercial_medical.SE))

  clean_FPsource <- standard_method_names(clean_FPsource) # Standardizing method names
  country_subnat_tbl <- clean_FPsource %>%
    dplyr::group_by(Country, Region) %>%
    dplyr::select(Country, Region) %>%
    dplyr::distinct() # table of country and regions (repeats in region names)
  n_method <- c("Female Sterilization", "Implants", "Injectables", "IUD", "OC Pills" ) # As per the method correlation matrix
  n_country <- unique(country_subnat_tbl$Country)
  n_subnat <- country_subnat_tbl$Region
  n_superregion <- unique(clean_FPsource$Super_region)

  clean_FPsource <- subnat_index_fun(clean_FPsource, n_subnat, country_subnat_tbl$Country) # Adding indexes to  data
  clean_FPsource <- country_index_fun(clean_FPsource, n_country)
  clean_FPsource <- method_index_fun(clean_FPsource, n_method)
  clean_FPsource <- superregion_index_fun(clean_FPsource, n_superregion)

  all_years <- seq(from = startyear, to = endyear, by=0.5) # 6-monthly increments
  n_all_years <- length(all_years)

  clean_FPsource <- clean_FPsource %>%
    dplyr::mutate(index_year = match(average_year,all_years)) # Time indexing - important for splines

  t_seq_2 <- floor(clean_FPsource$index_year) # Time sequence for countries
  country_seq <- clean_FPsource$Country
  n_country <- unique(country_subnat_tbl$Country)
  n_sector <- c("Public", "Commercial_medical", "Other") # Names of categories
  n_obs <- nrow(clean_FPsource) # Total number of observations
  year_seq <- seq(min(t_seq_2),max(t_seq_2), by=1)
  n_years <- length(year_seq) # Total number of years

  country_index_tbl <- clean_FPsource %>%
    dplyr::group_by(Country, index_country) %>%
    dplyr::select() %>%
    dplyr::distinct() # table of country and regions (repeats in region names)

  index_country_subnat_tbl <- clean_FPsource %>%
    dplyr::group_by(index_country, index_subnat) %>%
    dplyr::select(Region, index_subnat, Country, index_country) %>%
    dplyr::distinct() # table of country and regions (repeats in region names)

  index_superregion_tbl <- clean_FPsource %>%
    dplyr::group_by(index_superregion, index_country) %>%
    dplyr::select(Country, index_country, Super_region, index_superregion) %>%
    dplyr::distinct()

  match_superregion <- index_superregion_tbl$index_superregion

  match_country <- index_country_subnat_tbl$index_country

  match_years <- clean_FPsource$index_year # observation year indexes in the prediction years

  match_method <- clean_FPsource$index_method # method indexes from the data

  match_subnat <-  clean_FPsource$index_subnat #subnational region indexes from the data

  T_star <- clean_FPsource %>%
    dplyr::group_by(Country, Region) %>%
    dplyr::filter(index_year==max(index_year)) %>%
    dplyr::select(Country, Region, index_country, index_subnat, average_year, index_year) %>%
    dplyr::arrange(index_subnat) %>%
    dplyr::ungroup() %>%
    dplyr::select(index_country, index_subnat, average_year, index_year) %>%
    dplyr::distinct()

  nseg=nsegments   # Default is 12
  Kstar <-  vector()
  B.ik <- array(dim = c(length(n_subnat), length(all_years),nseg+3))
  knots.all <- matrix(nrow=length(n_subnat), ncol=nseg+3)

  for(i in 1:nrow(T_star)) {
    index_p <- T_star$index_subnat[i]
    index_msn <- T_star$average_year[i]
    res <- bs_bbase_precise(all_years, lastobs=index_msn, nseg = nseg) # number of splines based on segments, here choosing 10
    B.ik[index_p,,] <- res$B.ik
    Kstar[index_p] <- res$Kstar
    knots.all[index_p,] <- res$knots.k
  }

  K <- dim(res$B.ik)[2]
  H <- K-1

  if(local==FALSE) {
    mylist = list(data = clean_FPsource,
                  tstar = T_star$index_year,
                  kstar = Kstar,
                  B.ik = B.ik,
                  n_years = n_all_years,
                  n_obs = n_obs,
                  K = K,
                  H = H,
                  C_count = length(n_country),
                  P_count = length(n_subnat),
                  M_count = length(n_method),
                  R_count = length(n_superregion),
                  n_method = n_method, # As per the method correlation matrix
                  n_country = n_country,
                  n_subnat = n_subnat,
                  n_super_region = n_superregion,
                  all_years = all_years,
                  matchsuperregion = match_superregion,
                  matchsubnat = match_subnat,
                  matchcountry = match_country,
                  matchmethod = match_method,
                  matchyears = match_years )
  } else {
    mylist = list(data = clean_FPsource,
                  tstar = T_star$index_year,
                  kstar = Kstar,
                  B.ik = B.ik,
                  n_years = n_all_years,
                  n_obs = n_obs,
                  K = K,
                  H = H,
                  C_count = length(n_country),
                  P_count = length(n_subnat),
                  M_count = length(n_method),
                  R_count = length(n_superregion),
                  n_method = n_method, # As per the method correlation matrix
                  n_country = n_country,
                  n_subnat = n_subnat,
                  n_super_region = n_superregion,
                  all_years = all_years,
                  matchsuperregion = match_superregion,
                  matchsubnat = match_subnat,
                  matchcountry = match_country,
                  matchmethod = match_method,
                  matchyears = match_years
    )
  }
  return(mylist)
}

#' Get the r and z ratio point estimates from the separate chains of the JAGS model runs
#' R and Z are the intermediate parameters that are used to estimates the final proportions. See the model file for context.
#' @name get_subnational_P_point_estimates
#' @param pkg_data Output of the `mcmsupply::get_subnational_modelinputs()` function.
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @param n_chain Default is 2. Number of chains to run in your MCMC sample.
#' @return returns the point estimates for the jags model object
#' @noRd

get_subnational_P_point_estimates <- function(pkg_data, local=FALSE, mycountry=NULL, n_chain=2) {

  # Get model inputs for cross matching to estimates
  n_country <- pkg_data$n_country
  n_subnat <- pkg_data$n_subnat
  n_method <- pkg_data$n_method
  n_sector <- c("Public", "Commercial_medical", "Other")
  n_all_years <- pkg_data$n_years
  mydata <- pkg_data$data
  all_years <- pkg_data$all_years
  K <- pkg_data$K
  B.ik <- pkg_data$B.ik

  # Creating index tables for reference ------------------------------------------------------------
  subnat_index_table <- mydata %>%
    dplyr::select(Country, Region, index_subnat) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()
  country_index_table <- tibble::tibble(Country = n_country, index_country = unique(mydata$index_country))
  method_index_table <- tibble::tibble(Method = n_method, index_method = 1:5)
  sector_index_table <- tibble::tibble(Sector = n_sector, index_sector = 1:3)
  year_index_table <- tibble::tibble(average_year = all_years,
                                     index_year = 1:n_all_years,
                                     floored_year = floor(all_years))

  if(local==FALSE) { # global model estimates

    # Get ratio estimates. Saves to temp directory
    get_subnational_r_z_samples(pkg_data, local=FALSE, n_chain=n_chain)

    # Calculate proportions using the full posterior sample. Reads in the r and z variables using the temp directory
    get_subnational_global_P_samps()

    # Get point estimates for median, 95% and 80% credible intervals
    all_p_pub <- get_subnational_global_P_estimates("P_Public.RDS", subnat_index_table, method_index_table, "Public", year_index_table)
    all_p_CM <- get_subnational_global_P_estimates("P_CM.RDS", subnat_index_table, method_index_table, "Commercial_medical", year_index_table)
    all_p_other <- get_subnational_global_P_estimates("P_Other.RDS", subnat_index_table, method_index_table, "Other", year_index_table)

    # all_p_pub <- get_subnational_global_P_estimates(main_path, global_P_samps$P_public, subnat_index_table, method_index_table, "Public", year_index_table)
    # all_p_CM <- get_subnational_global_P_estimates(main_path, global_P_samps$P_CM, subnat_index_table, method_index_table, "Commercial_medical", year_index_table)
    # all_p_other <- get_subnational_global_P_estimates(main_path, global_P_samps$P_other, subnat_index_table, method_index_table, "Other", year_index_table)

    all_p <- rbind(all_p_pub, all_p_CM)
    all_p <- rbind(all_p, all_p_other)
  } else { # local model estimates
    # Read in chains results
    for(i in 1:n_chain) {
      f <- file.path(tempdir(), paste0(i,"chain.rds"))
      chain <- readRDS(f)
      chain <- chain$BUGSoutput$sims.matrix %>% tibble::as_tibble()
      if(i==1) {
        # Pull out P posterior samples for each of the three sectors
        public_samps <- chain[,stringr::str_detect(colnames(chain), "P\\[1,")] # init the tibble for chain=1
        CM_samps <- chain[,stringr::str_detect(colnames(chain), "P\\[2,")]
        other_samps <- chain[,stringr::str_detect(colnames(chain), "P\\[3,")]
      } else {
          public_samps <- dplyr::bind_rows(public_samps, chain[,stringr::str_detect(colnames(chain), "P\\[1,")])
          CM_samps <- dplyr::bind_rows(CM_samps, chain[,stringr::str_detect(colnames(chain), "P\\[2,")])
          other_samps <- dplyr::bind_rows(other_samps, chain[,stringr::str_detect(colnames(chain), "P\\[3,")])
      }
    }
    # chain1 <- readRDS(paste0(main_path,"/output/1chain.rds"))
    # chain1 <- chain1$BUGSoutput$sims.matrix %>% tibble::as_tibble()
    #
    # chain2 <- readRDS(paste0(main_path,"/output/2chain.rds"))
    # chain2 <- chain2$BUGSoutput$sims.matrix %>% tibble::as_tibble()
    #
    # # Pull out P posterior samples for each of the three sectors
    # public_samps <- dplyr::bind_rows(chain1[,stringr::str_detect(colnames(chain1), "P\\[1,")], chain2[,stringr::str_detect(colnames(chain2), "P\\[1,")])
    # CM_samps <- dplyr::bind_rows(chain1[,stringr::str_detect(colnames(chain1), "P\\[2,")], chain2[,stringr::str_detect(colnames(chain2), "P\\[2,")])
    # other_samps <- dplyr::bind_rows(chain1[,stringr::str_detect(colnames(chain1), "P\\[3,")], chain2[,stringr::str_detect(colnames(chain2), "P\\[3,")])

    # Get point estimates for median, 95% and 80% credible intervals
    pub_df <- get_subnational_local_P_estimates(public_samps, colnames(public_samps), subnat_index_table, method_index_table, sector_index_table, year_index_table)
    CM_df <- get_subnational_local_P_estimates(CM_samps, colnames(CM_samps), subnat_index_table, method_index_table, sector_index_table, year_index_table)
    other_df <- get_subnational_local_P_estimates(other_samps, colnames(other_samps), subnat_index_table, method_index_table, sector_index_table, year_index_table)

    all_p <- dplyr::bind_rows(pub_df, CM_df, other_df)
  }

  if(is.null(mycountry)==TRUE) {
    f <- file.path(tempdir(), "P_point_estimates.RDS")
    saveRDS(all_p, f)
  } else {
    f <- file.path(tempdir(), paste0(mycountry,"_P_point_estimates.RDS"))
    saveRDS(all_p, f)
  }
  return(all_p)
}

#' Get the r and z ratio point estimates from the separate chains of the JAGS model runs
#' R and Z are the intermediate parameters that are used to estimates the final proportions. See the model file for context.
#' @name get_subnational_r_z_samples
#' @param pkg_data Output of the `mcmsupply::get_subnational_modelinputs()` function.
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param n_chain Default is 2. Number of chains to run in your MCMC sample.
#' @return returns the point estimates for the jags model object
#' @noRd

get_subnational_r_z_samples <- function(pkg_data, local=FALSE, n_chain) {

  for(i in 1:n_chain) {
    f1 <- file.path(tempdir(), paste0(i,"chain.rds"))
    chain1 <- readRDS(f1)
    chain1 <- chain1$BUGSoutput$sims.matrix %>% tibble::as_tibble()

    # f2 <- file.path(tempdir(), "2chain.rds")
    # chain2 <- readRDS(f2)
    # chain2 <- chain2$BUGSoutput$sims.matrix %>% tibble::as_tibble()

    # Create P estimates --------------------------------------
    n_samps <- 2000
    P_count = length(pkg_data$n_subnat)
    M_count = length(pkg_data$n_method)
    S_count = length(pkg_data$n_sector)
    n_all_years <- length(pkg_data$all_years)
    K <- pkg_data$K
    B.ik <- pkg_data$B.ik

    z <- r <- array(dim = c(n_chain*n_samps, M_count, P_count, n_all_years))
    z_tmp <- r_tmp <- array(dim = c(n_samps, M_count, P_count, n_all_years))

    for(m in 1:M_count){ # method loop
      for(p in 1:P_count) { # province loop matched to C
        for (t in 1:n_all_years) {
          # get column names
          alphapub_param <- paste0("alpha_pms[",1,",",m,",",p,"]")
          alphapriv_param <- paste0("alpha_pms[",2,",",m,",",p,"]")

          betakpub_param <- paste0("beta.k[",1,",",m,",",p,",",paste0(1:K,"]"))
          betakpriv_param <- paste0("beta.k[",2,",",m,",",p,",",paste0(1:K,"]"))

          # chain 1 public and private
          alpha_sampspub1 <- chain1[,which(colnames(chain1)==alphapub_param)] %>% unlist() %>% as.vector()
          beta_sampspub1 <- chain1[,which(colnames(chain1) %in% betakpub_param)] %>% as.matrix()
          alpha_sampspriv1 <- chain1[,which(colnames(chain1)==alphapriv_param)] %>% unlist() %>% as.vector()
          beta_sampspriv1 <- chain1[,which(colnames(chain1) %in% betakpriv_param)] %>% as.matrix()

          # # chain 2 public and private
          # alpha_sampspub2 <- chain2[,which(colnames(chain2)==alphapub_param)] %>% unlist() %>% as.vector()
          # beta_sampspub2 <- chain2[,which(colnames(chain2) %in% betakpub_param)] %>% as.matrix()
          # alpha_sampspriv2 <- chain2[,which(colnames(chain2)==alphapriv_param)] %>% unlist() %>% as.vector()
          # beta_sampspriv2 <- chain2[,which(colnames(chain2) %in% betakpriv_param)] %>% as.matrix()

          z_tmp[1:n_samps,m,p,t] <- alpha_sampspub1 + (B.ik[p,t,] %*% t(beta_sampspub1))
          #z[(n_samps+1):(2*n_samps),m,p,t] <- alpha_sampspub2 + (B.ik[p,t,] %*% t(beta_sampspub2))
          # private sector
          r_tmp[1:n_samps,m,p,t] <- alpha_sampspriv1 + (B.ik[p,t,] %*% t(beta_sampspriv1))
          #r[(n_samps+1):(2*n_samps),m,p,t] <- alpha_sampspriv2 + (B.ik[p,t,] %*% t(beta_sampspriv2))
        } # end time loop
      } # end M loop
    } # end P loop
    start <- ((n_chain-1)*n_samps)+1
    end <- (n_chain*n_samps)
    z[start:end,,,] <- z_tmp
    r[start:end,,,] <- r_tmp
    }
    fz <- file.path(tempdir(), paste0("zsamps.RDS"))
    saveRDS(z, fz)
    fr <- file.path(tempdir(), paste0("rsamps.RDS"))
    saveRDS(r, fr)
  return(list(r = r,
              z = z))
}


#' Apply method indexing to data
#' @name method_index_fun
#' @param my_data The sub-national family planning source data for the country of interest.
#' @param my_methods A vector of your contraceptive methods in the order you wished them indexed.
#' @return Dataframe with method indexing applied
#' @noRd

method_index_fun <- function(my_data, my_methods) {
  my_data$index_method <- rep(NA, nrow(my_data))
  for (i in 1:length(my_methods)) {
    for (j in 1:nrow(my_data)) {
      method_name <- my_methods[i]
      if(my_data$Method[j]==method_name) {
        my_data$index_method[j] <- i
      }
      else {
        next
      }
    }
  }
  return(my_data)
}

#' Plot median and 95% credible intervals for posterior samples of P from JAGS model with the relevant survey data
#' @name plot_national_point_estimates
#' @param pkg_data Output of the `mcmsupply::get_subnational_modelinputs()` function.
#' @param model_output The output of the `mcmsupply::run_jags_model()` function.
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @return List of ggplot objects arranged by country name.
#' @noRd

plot_national_point_estimates <- function(pkg_data, model_output, local=FALSE, mycountry=NULL) {

  # Get models estimates
  estimates <- model_output$estimates

  # Get model inputs for cross matching to estimates
  n_country <- pkg_data$n_country
  n_method <- pkg_data$n_method
  n_sector <- c("Public", "Commercial_medical", "Other")
  n_all_years <- pkg_data$n_years
  mydata <- pkg_data$data
  all_years <- pkg_data$all_years

  # Creating index tables for reference
  country_index_table <- tibble::tibble(Country = n_country, index_country = unique(mydata$index_country))
  method_index_table <- tibble::tibble(Method = n_method, index_method = 1:length(n_method))
  sector_index_table <- tibble::tibble(Sector = n_sector, index_sector = 1:length(n_sector))
  year_index_table <- tibble::tibble(average_year = all_years,
                                     index_year = 1:n_all_years,
                                     floored_year = floor(all_years))

  # Recreating data used in model for plotting
  FP_source_data_long_SE <- mydata %>%
    dplyr::select(Country, Method, average_year, Commercial_medical.SE, Public.SE, Other.SE) %>%
    dplyr::rename(Commercial_medical = Commercial_medical.SE , Public = Public.SE, Other = Other.SE) %>%
    tidyr::gather(Sector, SE.proportion, Commercial_medical:Other, factor_key=TRUE)

  FP_source_data_long <- mydata %>%
    dplyr::select(Country, Method, average_year, Commercial_medical, Public, Other) %>%
    tidyr::gather(Sector, proportion, Commercial_medical:Other, factor_key=TRUE)

  FP_source_data_long <- merge(FP_source_data_long, FP_source_data_long_SE)

  # Adding error limits to data using SE
  FP_source_data_long <- FP_source_data_long %>%
    dplyr::mutate( prop_min = proportion - 2*SE.proportion,
                   prop_max = proportion + 2*SE.proportion ) %>%
    dplyr::mutate(prop_max = ifelse(prop_max > 1, 1, prop_max)) %>%
    dplyr::mutate(prop_min = ifelse(prop_min < 0, 0, prop_min))

  # Country estimates
  safe_colorblind_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  plot_list<- list()
  for(i in n_country) {
    print(i)
    country_data <- FP_source_data_long[which(FP_source_data_long$Country==i), ] #%>% filter(sector_category=="Public")
    country_calc <- estimates[which(estimates$Country==i), ] #%>% filter(sector_category=="Public")

    ci_plot = ggplot2::ggplot() +  # plot of true p value vs time using facet wrap
      ggplot2::geom_line(data=country_calc, ggplot2::aes(x=average_year, y=median_p, color=Sector)) +
      ggplot2::geom_point(data=country_data, ggplot2::aes(x=average_year, y=proportion, colour=Sector))+
      ggplot2::geom_errorbar(data=country_data, ggplot2::aes(ymin = prop_min, ymax = prop_max, x=average_year, colour=Sector), width = 1.5) +
      ggplot2::geom_ribbon(data=country_calc, ggplot2::aes(ymin = lower_95, ymax = upper_95, x=average_year, fill=Sector), alpha=0.2) +
      ggplot2::geom_ribbon(data=country_calc, ggplot2::aes(ymin = lower_80, ymax = upper_80, x=average_year, fill=Sector), alpha=0.26) +
      ggplot2::labs(y="Proportion of contraceptives supplied", x = "Year", title = i) +
      ggplot2::scale_y_continuous(limits=c(0,1))+
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), strip.text.x = ggplot2::element_text(size = 9)) +
      ggplot2::theme(legend.position = "bottom")+
      ggplot2::labs(fill = "Sector") +
      ggplot2::guides(color="none") +
      ggplot2::scale_colour_manual(values=safe_colorblind_palette) +
      ggplot2::scale_fill_manual(values=safe_colorblind_palette) +
      ggplot2::facet_wrap(~Method)

    country_name <- i

    plot_list[[i]] <- ci_plot
  }
  return(plot_list)
}

#' Plot median, 95% and 80% credible intervals for posterior samples of P from local JAGS model with the relevant survey data
#' @name plot_subnational_point_estimates
#' @param pkg_data Output of the `mcmsupply::get_subnational_modelinputs()` function.
#' @param model_output The output of the `mcmsupply::run_jags_model()` function.
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @return List of ggplot objects arranged by country and subnational region name.
#' @noRd

plot_subnational_point_estimates <- function(pkg_data, model_output, local=FALSE, mycountry=NULL) {

  # Get models estimates
  estimates <- model_output$estimates

  # Get model inputs for cross matching to estimates
  n_country <- pkg_data$n_country
  n_subnat <- pkg_data$n_subnat
  n_method <- pkg_data$n_method
  n_sector <- c("Public", "Commercial_medical", "Other")
  n_all_years <- pkg_data$n_years
  mydata <- pkg_data$data
  all_years <- pkg_data$all_years

  # Creating index tables for reference
  subnat_index_table <- mydata %>%
    dplyr::select(Country, Region, index_subnat) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()
  country_index_table <- tibble::tibble(Country = n_country, index_country = unique(mydata$index_country))
  method_index_table <- tibble::tibble(Method = n_method, index_method = 1:length(n_method))
  sector_index_table <- tibble::tibble(Sector = n_sector, index_sector = 1:length(n_sector))
  year_index_table <- tibble::tibble(average_year = all_years,
                                     index_year = 1:n_all_years,
                                     floored_year = floor(all_years))

  # Recreating data used in model for plotting
  FP_source_data_long_SE <- mydata %>%
    dplyr::select(Country, Region, Method, average_year, Commercial_medical.SE, Public.SE, Other.SE) %>%
    dplyr::rename(Commercial_medical = Commercial_medical.SE , Public = Public.SE, Other = Other.SE) %>%
    tidyr::gather(Sector, SE.proportion, Commercial_medical:Other, factor_key=TRUE)

  FP_source_data_long <- mydata %>%
    dplyr::select(Country, Region, Method, average_year, Commercial_medical, Public, Other) %>%
    tidyr::gather(Sector, proportion, Commercial_medical:Other, factor_key=TRUE)

  FP_source_data_long <- merge(FP_source_data_long, FP_source_data_long_SE)

  # Adding error limits to data using SE
  FP_source_data_long <- FP_source_data_long %>%
    dplyr::mutate( prop_min = proportion - 2*SE.proportion,
                   prop_max = proportion + 2*SE.proportion ) %>%
    dplyr::mutate(prop_max = ifelse(prop_max > 1, 1, prop_max)) %>%
    dplyr::mutate(prop_min = ifelse(prop_min < 0, 0, prop_min))

  # Country estimates
  safe_colorblind_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  plot_list <- list()
  for(i in 1:nrow(subnat_index_table)) {
    country_data <- FP_source_data_long %>%
      dplyr::filter(Country==subnat_index_table$Country[i] & Region==subnat_index_table$Region[i]) %>%
      dplyr::rename(Sector = Sector)
    country_calc <- estimates %>%
      dplyr::filter(Country==subnat_index_table$Country[i] & Region==subnat_index_table$Region[i])

    ci_plot = ggplot2::ggplot() +  # plot of true p value vs time using facet wrap
      ggplot2::geom_line(data=country_calc, ggplot2::aes(x=average_year, y=median_p, color=Sector)) +
      ggplot2::geom_point(data=country_data, ggplot2::aes(x=average_year, y=proportion, colour=Sector))+
      ggplot2::geom_errorbar(data=country_data, ggplot2::aes(ymin = prop_min, ymax = prop_max, x=average_year, colour=Sector), width = 1.5) +
      ggplot2::geom_ribbon(data=country_calc, ggplot2::aes(ymin = lower_95, ymax = upper_95, x=average_year, fill=Sector), alpha=0.2) +
      ggplot2::geom_ribbon(data=country_calc, ggplot2::aes(ymin = lower_80, ymax = upper_80, x=average_year, fill=Sector), alpha=0.26) +
      ggplot2::labs(y="Proportion of contraceptives supplied", x = "Year", title = paste0(unique(country_calc$Region),", ",unique(country_data$Country))) +
      ggplot2::scale_y_continuous(limits=c(0,1))+
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom")+
      ggplot2::scale_colour_manual(values=safe_colorblind_palette) +
      ggplot2::scale_fill_manual(values=safe_colorblind_palette) +
      ggplot2::labs(fill = "Sector") +
      ggplot2::guides(color="none") +
      ggplot2::theme(axis.text = ggplot2::element_text(angle = 90), strip.text.x = ggplot2::element_text(size = 12)) +
      #ggplot2::theme(legend.title = ggplot2::element_text(size=20), legend.key.size = ggplot2::unit(2.5, 'cm'), legend.text = ggplot2::element_text(size=20)) +
      ggplot2::facet_wrap(~Method)

    country_name <- subnat_index_table$Country[i]
    region_name <- stringr::str_replace_all(subnat_index_table$Region[i], "[[:punct:]]", "_") # remove special characters
    region_name <- stringr::str_replace_all(region_name, " ", "") # remove spaces
    plot_list[[paste0(country_name, "_", region_name)]] <- ci_plot
  }
  return(plot_list)
}

#' Indexing function for regions used in data.
#' @name region_index_fun
#' @param my_data The data with a Region column you want to index.
#' @param  n_region A vector of regions used in the data
#' @return returns data with region indexed
#' @noRd

region_index_fun <- function(my_data, n_region) {
  my_data$index_region <- NA
  for (i in 1:length(n_region)) {
    for (j in 1:nrow(my_data)) {
      region_name <- n_region[i]
      if(my_data$Region[j]==region_name) {
        my_data$index_region[j] <- i
      }
      else {
        next
      }
    }
  }
  return(my_data)
}

#' Run the jags model for estimating the proportion of modern contraceptive methods supplied by the public & private Sectors using a Bayesian hierarchical penalized spline model
#' @name run_national_jags_model
#' @param jagsdata The inputs for the JAGS model
#' @param jagsparams The parameters of the JAGS model you wish to review
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param n_iter Default is 80000. Number of itterations to do in JAGS model.
#' @param n_burnin Default is 10000. Number of samples to burn-in in JAGS model.
#' @param n_thin Default is 35. Number of samples to thin by in JAGS model.
#' @param n_chain Default is 2. Number of chains to run in your MCMC sample.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @return returns the jags model object
#' @noRd

run_national_jags_model <- function(jagsdata, jagsparams = NULL, local=FALSE,
                                    n_iter = 80000, n_burnin = 10000, n_thin = 35, n_chain = 2, mycountry=NULL) {

  # Get default data input list for JAGS
  myjagsdata <- get_national_JAGSinput_list(jagsdata, local= local,  mycountry=mycountry)

  # Get default parameters to monitor
  if(is.null(jagsparams)==TRUE ) {
    if(local==FALSE) { # global
      jagsparams <- c("P",
                      "beta.k",
                      "alpha_cms")
    } else { # local
      jagsparams <- c("P",
                      "alpha_cms",
                      "beta.k",
                      "inv.sigma_delta")
    }
  }
  # write JAGS model
  #write_jags_model(main_path=main_path, model_type = "national", local=local)

  if(local==TRUE & is.null(mycountry)==FALSE) {
    mod <- R2jags::jags(data=myjagsdata,
                        parameters.to.save=jagsparams,
                        model.file = system.file("model", "local_national_model.txt", package = "mcmsupply"), #system.file(main_path, "/model.txt", package = "mcmsupply"),
                        n.burnin = n_burnin,
                        n.iter = n_iter,
                        n.thin = n_thin,
                        n.chain = n_chain)
    f <- file.path(tempdir(), paste0("mod_",mycountry,"_national_results.RDS"))
    saveRDS(mod, f)
  } else {
    mod <- R2jags::jags(data=myjagsdata,
                        parameters.to.save=jagsparams,
                        model.file = system.file("model", "global_national_model.txt", package = "mcmsupply"), #system.file(main_path, "/model.txt", package = "mcmsupply"),
                        n.burnin = n_burnin,
                        n.iter = n_iter,
                        n.thin = n_thin,
                        n.chain = n_chain)
    f <- file.path(tempdir(), "mod_global_national_results.RDS")
    saveRDS(mod, f)
  }
  return(mod)
}

#' Run the jags model for estimating the proportion of modern contraceptive methods supplied by the public & private sectors at a subnational level using a Bayesian hierarchical penalized spline model
#' @name run_subnational_jags_model
#' @param jagsdata The inputs for the JAGS model
#' @param jagsparams The parameters of the JAGS model you wish to review
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param n_iter Default is 80000. Number of itterations to do in JAGS model.
#' @param n_burnin Default is 10000. Number of samples to burn-in in JAGS model.
#' @param n_thin Default is 35. Number of samples to thin by in JAGS model.
#' @param n_chain Default is 2. Number of chains to run in your MCMC sample.
#' @param n_cores 	The number of cores to use for parallel execution. If not specified, the number of cores is set to the value of options("cores"), if specified, or to approximately half the number of cores detected by the parallel package.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @return returns the jags model object
#' @noRd

run_subnational_jags_model <- function(jagsdata, jagsparams = NULL, local=FALSE,
                                       n_iter = 80000, n_burnin = 10000, n_thin = 35, n_chain = 2, n_cores=NULL, mycountry=NULL) {

  # Get JAGS input data list
  myjagsdata <- get_subnational_JAGSinput_list(jagsdata, local=local, mycountry=mycountry)

  # Get JAGS params to monitor
  if(is.null(jagsparams)==TRUE ) {
    if(local==FALSE) { # global
      jagsparams <- c("alpha_pms",
                      "alpha_cms",
                      "tau_alpha_pms",
                      "beta.k",
                      "sigma_delta",
                      "delta.k")
    } else { # local
      jagsparams <- c("P",
                      "alpha_pms",
                      "beta.k")
    }
  }

  # run JAGS model
 for(chain in 1:n_chain){ ## Do chains separately ------------------------------
    set.seed(chain*1239)
    if(local==FALSE) { # multi-country subnational
      mod <- R2jags::jags(data = myjagsdata,
                          parameters.to.save = jagsparams,
                          model.file = system.file("model", "global_subnational_model.txt", package = "mcmsupply"), #system.file(main_path, "/model.txt", package = "mcmsupply"),
                          n.chains = 1,
                          n.burnin = n_burnin,
                          n.iter = n_iter,
                          n.thin = n_thin)
    } else { # single country subnational
      mod <- R2jags::jags(data = myjagsdata,
                          parameters.to.save = jagsparams,
                          model.file = system.file("model", "local_subnational_model_fixed.txt", package = "mcmsupply"), #system.file(main_path, "/model.txt", package = "mcmsupply"),
                          n.chains = 1,
                          n.burnin = n_burnin,
                          n.iter = n_iter,
                          n.thin = n_thin)
    }
    mod$BUGSoutput[names(mod$BUGSoutput) %in% c("sims.list", # REMOVING SOME DATA FROM FIT OBJECT TO REDUCE FILE SIZE
                                                "summary",
                                                "mean",
                                                "sd")] <- NA
    f <- file.path(tempdir(), paste0(chain, "chain.rds"))
    saveRDS(mod, f)
    message(paste("MCMC results for chain ", chain, "complete"))
  } # end chains

  gc()
  chain=1
  f <- file.path(tempdir(), "1chain.rds")
  mod <- readRDS(f)
  if(n_chain >1) { # combine the separate chains together into one object
    for (chain in 2:n_chain) {
      f2 <- file.path(tempdir(), paste0(chain,"chain.rds"))
      mod_for_one_chain <-readRDS(f2)
      mod$BUGSoutput$sims.array <- mod$BUGSoutput$sims.array %>% abind::abind(mod_for_one_chain$BUGSoutput$sims.array, along = 2)
    }
  }

  # now we need to hack the fit object such that it has correct meta data (as the original git object had meta data for just 1 chain)
  mod$BUGSoutput$n.chains <- n_chain

  if(local==TRUE) { # save local subnational models
    f <- file.path(tempdir(), paste0("mod_local_subnational_",mycountry,".RDS"))
    saveRDS(mod, f)
  } else { # save global subnational models
    f <- file.path(tempdir(), "mod_global_subnational.RDS")
    saveRDS(mod, f)
  }
  return(mod)
}

#' Indexing function for sectors used in data.
#' @name sector_index_fun
#' @param my_data The data with a sector_category column you want to index.
#' @param  my_sectors A vector of sectors used in the data
#' @return returns data with sectors indexed
#' @noRd

sector_index_fun <- function(my_data, my_sectors) {
  for (i in 1:length(my_sectors)) {
    for (j in 1:nrow(my_data)) {
      sector_name <- my_sectors[i]
      if(my_data$sector_category[j]==sector_name) {
        my_data$index_sector[j] <- i
      }
      else {
        next
      }
    }
  }
  return(my_data)
}

#' Standardise the contraceptive method names
#' @name standard_method_names
#' @param my_data The input data with a Method column to standardise
#' @return Original input dataframe with Method column standardised.
#' @noRd

standard_method_names <- function(my_data) {
  my_data <- my_data %>%
    dplyr::mutate(Method = replace(Method, Method == "Condom (m+f)", "Condom")) %>%
    dplyr::mutate(Method = replace(Method, Method == "male condom", "Condom")) %>%
    dplyr::mutate(Method = replace(Method, Method == "condom", "Condom")) %>%
    dplyr::mutate(Method = replace(Method, Method == "Sterilization (female)", "Female Sterilization")) %>%
    dplyr::mutate(Method = replace(Method, Method == "female sterilization", "Female Sterilization")) %>%
    dplyr::mutate(Method = replace(Method, Method == "Female sterilization", "Female Sterilization")) %>%
    dplyr::mutate(Method = replace(Method, Method == "Sterilization (male)", "Male Sterilization")) %>%
    dplyr::mutate(Method = replace(Method, Method == "Male sterilization", "Male Sterilization")) %>%
    dplyr::mutate(Method = replace(Method, Method == "male sterilization", "Male Sterilization")) %>%
    dplyr::mutate(Method = replace(Method, Method == "Pill", "OC Pills")) %>%
    dplyr::mutate(Method = replace(Method, Method == "pill", "OC Pills")) %>%
    dplyr::mutate(Method = replace(Method, Method == "injections", "Injectables")) %>%
    dplyr::mutate(Method = replace(Method, Method == "implants", "Implants")) %>%
    dplyr::mutate(Method = replace(Method, Method %in% c("Other", "Other Modern Methods", "LAM", "diaphragm", "female condom", "foam or jelly", "standard days method", "diaphragm, foam or jelly", "lactational amenorrhea", "emergency contraception", "other modern methods"), "Other Modern Methods"))

  return(my_data)
}

#' Apply subnational indexing to data
#' @name subnat_index_fun
#' @param my_data The sub-national family planning source data for the country of interest.
#' @param my_subnat A vector of the provinces/region names in your country of interest.
#' @param my_country The name of your country of interest. Must be a vector the same length as my_subnat.
#' @return Dataframe with subnational indexing applied
#' @noRd

subnat_index_fun <- function(my_data, my_subnat, my_country) {
  for (i in 1:length(my_subnat)) {
    for (j in 1:nrow(my_data)) {
      subnat_name <- my_subnat[i]
      country_name <- my_country[i]
      if(my_data$Country[j]==country_name & my_data$Region[j]==subnat_name) {
        my_data$index_subnat[j] <- i
      }
      else {
        next
      }
    }
  }
  return(my_data)
}

#' Apply subcontinent indexing to data
#' @name superregion_index_fun
#' @param my_data National level family planning source data
#' @param n_region A vector of subcontinents listed in the dataset
#' @return Dataframe with subconintental indexing applied
#' @noRd

superregion_index_fun <- function(my_data, n_region) {
  my_data$index_superregion <- NA
  for (i in 1:length(n_region)) {
    for (j in 1:nrow(my_data)) {
      region_name <- n_region[i]
      if(my_data$Super_region[j]==region_name) {
        my_data$index_superregion[j] <- i
      }
      else {
        next
      }
    }
  }
  return(my_data)
}

#' @name write_jags_model
#' @param main_path String. Path where you have set your model results to be saved to.
#' @param model_type String. Two options, "subnational" for subnational-level JAGS code or "national" for national-level JAGS code.
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @return returns a list ready for input into the JAGS model
#' @noRd

write_jags_model <- function(main_path, model_type, local=FALSE) {
  if(model_type == "national") {
    if(local==FALSE) { # global national model
      cat(
        "model{
## Variance structure
  for(g in 1:2) { # Sector (public,private) loop for rho
    for(i in 1:M_count){ # Method loop i
      mu_delta[g,i] <- 0
      sd_delta[g,i] ~ dunif(0,1)
      sigma_delta[i,i,g] <- pow(sd_delta[g,i],2)
    }

    sigma_delta[1,2,g] <- rho[1,g]*sd_delta[g,1]*sd_delta[g,2]
    sigma_delta[1,3,g] <- rho[2,g]*sd_delta[g,1]*sd_delta[g,3]
    sigma_delta[1,4,g] <- rho[3,g]*sd_delta[g,1]*sd_delta[g,4]
    sigma_delta[1,5,g] <- rho[4,g]*sd_delta[g,1]*sd_delta[g,5]
    sigma_delta[2,1,g] <- rho[1,g]*sd_delta[g,1]*sd_delta[g,2]
    sigma_delta[2,3,g] <- rho[5,g]*sd_delta[g,2]*sd_delta[g,3]
    sigma_delta[2,4,g] <- rho[6,g]*sd_delta[g,2]*sd_delta[g,4]
    sigma_delta[2,5,g] <- rho[7,g]*sd_delta[g,2]*sd_delta[g,5]
    sigma_delta[3,1,g] <- rho[2,g]*sd_delta[g,1]*sd_delta[g,3]
    sigma_delta[3,2,g] <- rho[5,g]*sd_delta[g,2]*sd_delta[g,3]
    sigma_delta[3,4,g] <- rho[8,g]*sd_delta[g,3]*sd_delta[g,4]
    sigma_delta[3,5,g] <- rho[9,g]*sd_delta[g,3]*sd_delta[g,5]
    sigma_delta[4,1,g] <- rho[3,g]*sd_delta[g,1]*sd_delta[g,4]
    sigma_delta[4,2,g] <- rho[6,g]*sd_delta[g,2]*sd_delta[g,4]
    sigma_delta[4,3,g] <- rho[8,g]*sd_delta[g,3]*sd_delta[g,4]
    sigma_delta[4,5,g] <- rho[10,g]*sd_delta[g,4]*sd_delta[g,5]
    sigma_delta[5,1,g] <- rho[4,g]*sd_delta[g,1]*sd_delta[g,5]
    sigma_delta[5,2,g] <- rho[7,g]*sd_delta[g,2]*sd_delta[g,5]
    sigma_delta[5,3,g] <- rho[9,g]*sd_delta[g,3]*sd_delta[g,5]
    sigma_delta[5,4,g] <- rho[10,g]*sd_delta[g,4]*sd_delta[g,5]
  } # end G loop

  inv.sigma_delta[1:M_count,1:M_count,1] <- inverse(sigma_delta[,,1])
  inv.sigma_delta[1:M_count,1:M_count,2] <- inverse(sigma_delta[,,2])


## Model Estimates
  for(c in 1:C_count) { # country loop
    for(m in 1:M_count){ # method loop
      for (t in 1:n_years) {
        z[m,c,t] <- alpha_cms[1,m,c] + inprod(B.ik[c,t,],beta.k[1,m,c,])
        r[m,c,t] <- alpha_cms[2,m,c] + inprod(B.ik[c,t,],beta.k[2,m,c,])
      } # end time loop
    } # end M loop
  } # end C loop

## Parameter Estimates
  for(s in 1:2){ # sector loop
    for(c in 1:C_count) { # country loop
      for(m in 1:M_count){ # method loop
        alpha_cms[s,m,c] ~ dnorm(beta_r[s,m,matchregion[c]],tau_alpha[s]) # sharing info across methods within a country so each country public/private sector has an intercept. Tau-alpha is the cross-method variance.
        beta.k[s,m,c,kstar[c]] <- 0 # kstar[c] is the knot at tstar for country c. Just the value of the intercept
        for(j in 1:(kstar[c]-1)){ # before kstar[c]
          beta.k[s,m,c,(kstar[c] - j)] <- beta.k[s,m,c,(kstar[c] - j)+1] - delta.k[s,m,c,(kstar[c] - j)]
        } # end K1 loop
        for(j in (kstar[c]+1):K){ # after kstar[c]
          beta.k[s,m,c,j] <- beta.k[s,m,c,(j-1)] + delta.k[s,m,c,(j-1)]
        } # end K2 loop
      } # end m loop

      for(j in 1:H){
        delta.k[s,c(1:M_count),c,j] ~ dmnorm(mu_delta[s,],inv.sigma_delta[,,s]) # delta are the slopes for country c and method m
      } # end H loop
    } # end C loop
  } # end S loop

## Estimating all the Categories here (including total private)
  for(c in 1:C_count){ # country loop
    for(m in 1:M_count){ # method loop
      for (t in 1:n_years) { # years loop
         P[1,m,c,t] <- 1/(1+exp(-(z[m,c,t]))) # modelling this as before assuming that z[m,c,t] is log(pi_public/pi_private)

         Q[m,c,t] <- 1/(1+exp(-(r[m,c,t]))) # logit-inverse of ratio

         U[m,c,t] <- 1-P[1,m,c,t] # this then gives you the total private sector

         P[2,m,c,t] <- Q[m,c,t]*U[m,c,t] # this is assuming that the logit(P[2,m,c,t]/U[m,c,t]) = r[m,c,t] i.e., we are modelling the ratio of private medical to private

         P[3,m,c,t] <- U[m,c,t] - P[2,m,c,t] # other = private - private medical
      }
    }
  }

## Likelihood
  for (k in 1:n_obs) {
    y[k, 1] ~ dnorm(P[1,matchmethod[k],matchcountry[k],matchyears[k]], tau_y[k,1])T(0,1)
    y[k, 2] ~ dnorm(P[2,matchmethod[k],matchcountry[k],matchyears[k]], tau_y[k,2])T(0,1)

    tau_y[k,1:2] <- 1/(se_prop[k,1:2]^2)
    }

## Priors
  for(s in 1:2) { # cross method variance (within a country)
    for(m in 1:M_count){ # method loop
      for(r in 1:R_count){ # regional
        beta_r[s,m,r] ~ dnorm(beta_world[s,m],tau_beta[s])
      }
       # world intercept
       beta_world[s,m] ~ dnorm(0,0.1)
    }
    tau_alpha[s] <- sigma_alpha[s]^-2
    sigma_alpha[s] ~ dt(0,1,1)T(0,)

    tau_beta[s] <- sigma_beta[s]^-2
    sigma_beta[s] ~ dt(0,1,1)T(0,)
  }
}", file=paste0(main_path,"/model.txt"))
    } else { # national local
      cat("model{
## Variance structure
  for(g in 1:2) { # Sector (public,crivate) loop for covariance
    mu_delta[g,1:M_count] <- rep(0,M_count)
    inv.sigma_delta[1:M_count,1:M_count,g] ~ dwish(natdf*natRmat[1:M_count,1:M_count,g],natdf) # S~dwish(R,c) => E(S) = p * solve(R)
  }

## Model Estimates
for(m in 1:M_count){ # method loop
  for (t in 1:n_years) {
    z[m,t] <- alpha_cms[1,m] + inprod(B.ik[t,],beta.k[1,m,])
    r[m,t] <- alpha_cms[2,m] + inprod(B.ik[t,],beta.k[2,m,])
  } # end **after tstar** loop
} # end M loop

## Parameter Estimates
  for(s in 1:2){ # sector loop
    for(m in 1:M_count){ # method loop
      alpha_cms[s,m] ~ dnorm(alphahat_region[s,m], tau_alphahat_cms[s]) # sharing info across methods within a subnat area so each subnat public/private sector has an intercept. Tau-alpha is the cross-method sector variance.
      beta.k[s,m,kstar] <- 0 # kstar is the knot at tstar for country c. Just the value of the intercept
      for(j in 1:(kstar-1)){ # before kstar
        beta.k[s,m,(kstar - j)] <- beta.k[s,m,(kstar - j)+1] - delta.k[s,m,(kstar - j)]
      } # end K1 loop
      for(j in (kstar+1):K){ # after kstar
        beta.k[s,m,j] <- beta.k[s,m,(j-1)] + delta.k[s,m,(j-1)]
      } # end K2 loop
    } # end m loop
    for(j in 1:H){
      delta.k[s,c(1:M_count),j] ~ dmnorm(mu_delta[s,],inv.sigma_delta[,,s]) # delta are the slopes for country c and method m
    } # end H loop
  } # end S loop

## Estimating all the Categories here (including total private)
  for(m in 1:M_count){ # method loop
    for (t in 1:n_years) { # years loop
      P[1,m,t] <- 1/(1+exp(-(z[m,t]))) # modelling this as before assuming that z[m,c,t] is log(pi_public/pi_private)
      Q[m,t] <- 1/(1+exp(-(r[m,t]))) # logit-inverse of ratio
      U[m,t] <- 1-P[1,m,t] # this then gives you the total private sector
      P[2,m,t] <- Q[m,t]*U[m,t] # this is assuming that the logit(P[2,m,c,t]/U[m,c,t]) = r[m,c,t] i.e., we are modelling the ratio of private medical to private
      P[3,m,t] <- U[m,t] - P[2,m,t] # other = private - private medical
      }
    }

## Likelihood
  for (k in 1:n_obs) {
    y[k, 1] ~ dnorm(P[1,matchmethod[k],matchyears[k]], tau_y[k,1])T(0,1)
    y[k, 2] ~ dnorm(P[2,matchmethod[k],matchyears[k]], tau_y[k,2])T(0,1)
    tau_y[k,1:2] <- 1/(se_prop[k,1:2]^2)
  }
}", file=paste0(main_path,"/model.txt"))
    }
  } else {
    if(model_type=="subnational") {
      if(local==FALSE) { # subnational global nonspatial
        cat("model{
## Variance structure
  for(g in 1:2) { # Sector (public,private) loop for rho
    for(i in 1:M_count){ # Method loop i
      mu_delta[g,i] <- 0
      sd_delta[g,i] ~ dt(0,1/25,1)T(0,)
      sigma_delta[i,i,g] <- pow(sd_delta[g,i],2)
    }

    sigma_delta[1,2,g] <- rho[1,g]*sd_delta[g,1]*sd_delta[g,2]
    sigma_delta[1,3,g] <- rho[2,g]*sd_delta[g,1]*sd_delta[g,3]
    sigma_delta[1,4,g] <- rho[3,g]*sd_delta[g,1]*sd_delta[g,4]
    sigma_delta[1,5,g] <- rho[4,g]*sd_delta[g,1]*sd_delta[g,5]
    sigma_delta[2,1,g] <- rho[1,g]*sd_delta[g,1]*sd_delta[g,2]
    sigma_delta[2,3,g] <- rho[5,g]*sd_delta[g,2]*sd_delta[g,3]
    sigma_delta[2,4,g] <- rho[6,g]*sd_delta[g,2]*sd_delta[g,4]
    sigma_delta[2,5,g] <- rho[7,g]*sd_delta[g,2]*sd_delta[g,5]
    sigma_delta[3,1,g] <- rho[2,g]*sd_delta[g,1]*sd_delta[g,3]
    sigma_delta[3,2,g] <- rho[5,g]*sd_delta[g,2]*sd_delta[g,3]
    sigma_delta[3,4,g] <- rho[8,g]*sd_delta[g,3]*sd_delta[g,4]
    sigma_delta[3,5,g] <- rho[9,g]*sd_delta[g,3]*sd_delta[g,5]
    sigma_delta[4,1,g] <- rho[3,g]*sd_delta[g,1]*sd_delta[g,4]
    sigma_delta[4,2,g] <- rho[6,g]*sd_delta[g,2]*sd_delta[g,4]
    sigma_delta[4,3,g] <- rho[8,g]*sd_delta[g,3]*sd_delta[g,4]
    sigma_delta[4,5,g] <- rho[10,g]*sd_delta[g,4]*sd_delta[g,5]
    sigma_delta[5,1,g] <- rho[4,g]*sd_delta[g,1]*sd_delta[g,5]
    sigma_delta[5,2,g] <- rho[7,g]*sd_delta[g,2]*sd_delta[g,5]
    sigma_delta[5,3,g] <- rho[9,g]*sd_delta[g,3]*sd_delta[g,5]
    sigma_delta[5,4,g] <- rho[10,g]*sd_delta[g,4]*sd_delta[g,5]
  } # end G loop

  inv.sigma_delta[1:M_count,1:M_count,1] <- inverse(sigma_delta[,,1])
  inv.sigma_delta[1:M_count,1:M_count,2] <- inverse(sigma_delta[,,2])

## Model Estimates
for(p in 1:P_count) { # province loop matched to C
  for(m in 1:M_count){ # method loop
    for (t in 1:n_years) {
      z[m,p,t] <- alpha_pms[1,m,p] + inprod(B.ik[p,t,],beta.k[1,m,p,])
      r[m,p,t] <- alpha_pms[2,m,p] + inprod(B.ik[p,t,],beta.k[2,m,p,])
    } # end time loop
  } # end M loop
} # end P loop


## Parameter Estimates
  for(s in 1:2){ # sector loop
    for(p in 1:P_count) { # province loop
      for(m in 1:M_count){ # method loop
        alpha_pms[s,m,p] ~ dnorm(alpha_cms[s,m,matchcountry[p]],tau_alpha[s])
        beta.k[s,m,p,kstar[p]] <- 0
        for(j in 1:(kstar[p]-1)){ # before kstar[p]
          beta.k[s,m,p,(kstar[p] - j)] <- beta.k[s,m,p,(kstar[p] - j)+1] - delta.k[s,m,p,(kstar[p] - j)]
        } # end K1 loop
        for(j in (kstar[p]+1):K){ # after kstar[p]
          beta.k[s,m,p,j] <- beta.k[s,m,p,(j-1)] + delta.k[s,m,p,(j-1)]
        } # end K2 loop
      } # end m loop
      for(j in 1:H){
        delta.k[s,c(1:M_count),p,j] ~ dmnorm(mu_delta[s,],inv.sigma_delta[,,s])
      } # end H loop
    } # end P loop
  } # end S loop

## Estimating all the Categories here (including total private)
  for(p in 1:P_count){ # province loop
    for(m in 1:M_count){ # method loop
      for (t in 1:n_years) { # years loop
         P[1,m,p,t] <- 1/(1+exp(-(z[m,p,t]))) # modelling this as before assuming that z[m,p,t] is log(pi_public/pi_private)

         Q[m,p,t] <- 1/(1+exp(-(r[m,p,t]))) # logit-inverse of ratio

         U[m,p,t] <- 1-P[1,m,p,t] # this then gives you the total private sector

         P[2,m,p,t] <- Q[m,p,t]*U[m,p,t] # this is assuming that the logit(P[2,m,p,t]/U[m,p,t]) = r[m,p,t] i.e., we are modelling the ratio of private medical to private

         P[3,m,p,t] <- U[m,p,t] - P[2,m,p,t] # other = private - private medical
      }
    }
  }

## Likelihood
  for (k in 1:n_obs) {
    y[k, 1] ~ dnorm(P[1,matchmethod[k],matchsubnat[k],matchyears[k]], tau_y[k,1])T(0,1)
    y[k, 2] ~ dnorm(P[2,matchmethod[k],matchsubnat[k],matchyears[k]], tau_y[k,2])T(0,1)
    tau_y[k,1:2] <- 1/(se_prop[k,1:2]^2)
    }

## Priors
  for(s in 1:2) { # cross method variance (within a country)
    for(m in 1:M_count){ # method loop
      for(c in 1:C_count) { # country loop
        alpha_cms[s,m,c] ~ dnorm(beta_r[s,m,matchregion[c]],tau_alpha[s])
      }
      for(r in 1:R_count){ # regional
        beta_r[s,m,r] ~ dnorm(beta_world[s,m],tau_beta[s])
      }
       # world intercept
       beta_world[s,m] ~ dnorm(0,0.1)
    }
    tau_alpha[s] <- sigma_alpha[s]^-2
    sigma_alpha[s] ~ dt(0,1,1)T(0,)

    tau_beta[s] <- sigma_beta[s]^-2
    sigma_beta[s] ~ dt(0,1,1)T(0,)
  }
              }", file=paste0(main_path,"/model.txt"))
      } else { # subnational local nonspatial
        cat("model{
## Variance structure
  for(g in 1:2) { # Sector (public,private) loop for covariance
    mu_delta[g,1:M_count] <- rep(0,M_count)
    inv.sigma_delta[1:M_count,1:M_count,g] ~ dwish(natdf*natRmat[1:M_count,1:M_count,g],natdf) # S~dwish(R,p) => E(S) = p * solve(R)
  }

## Model Estimates
  for(p in 1:P_count) { # subnat loop
    for(m in 1:M_count){ # method loop
      for (t in 1:n_years) {
        z[m,p,t] <- alpha_pms[1,m,p] + inprod(B.ik[p,t,],beta.k[1,m,p,])
        r[m,p,t] <- alpha_pms[2,m,p] + inprod(B.ik[p,t,],beta.k[2,m,p,])
      } # end **after tstar** loop
    } # end M loop
  } # end P loop

## Parameter Estimates
  for(s in 1:2){ # sector loop
    for(p in 1:P_count) { # subnat loop
      for(m in 1:M_count){ # method loop
        alpha_pms[s,m,p] ~ dnorm(alpha_cms_hat[s,m], tau_alpha_pms_hat[s]) # sharing info across methods within a subnat area so each subnat public/private sector has an intercept. Tau-alpha is the cross-method sector variance.
        beta.k[s,m,p,kstar[p]] <- 0 # kstar[p] is the knot at tstar for country c. Just the value of the intercept
        for(j in 1:(kstar[p]-1)){ # before kstar[p]
          beta.k[s,m,p,(kstar[p] - j)] <- beta.k[s,m,p,(kstar[p] - j)+1] - delta.k[s,m,p,(kstar[p] - j)]
        } # end K1 loop
        for(j in (kstar[p]+1):K){ # after kstar[p]
          beta.k[s,m,p,j] <- beta.k[s,m,p,(j-1)] + delta.k[s,m,p,(j-1)]
        } # end K2 loop
      } # end m loop

      for(j in 1:H){
        delta.k[s,c(1:M_count),p,j] ~ dmnorm(mu_delta[s,],inv.sigma_delta[,,s]) # delta are the slopes for country c and method m
      } # end H loop

    } # end C loop
  } # end S loop

## Estimating all the Categories here (including total private)
  for(p in 1:P_count){ # country loop
    for(m in 1:M_count){ # method loop
      for (t in 1:n_years) { # years loop
         P[1,m,p,t] <- 1/(1+exp(-(z[m,p,t]))) # modelling this as before assuming that z[m,p,t] is log(pi_public/pi_private)

         Q[m,p,t] <- 1/(1+exp(-(r[m,p,t]))) # logit-inverse of ratio

         U[m,p,t] <- 1-P[1,m,p,t] # this then gives you the total private sector

         P[2,m,p,t] <- Q[m,p,t]*U[m,p,t] # this is assuming that the logit(P[2,m,p,t]/U[m,p,t]) = r[m,p,t] i.e., we are modelling the ratio of private medical to private

         P[3,m,p,t] <- U[m,p,t] - P[2,m,p,t] # other = private - private medical
      }
    }
  }

## Likelihood
  for (k in 1:n_obs) {
    y[k, 1] ~ dnorm(P[1,matchmethod[k],matchsubnat[k],matchyears[k]], tau_y[k,1])T(0,1)
    y[k, 2] ~ dnorm(P[2,matchmethod[k],matchsubnat[k],matchyears[k]], tau_y[k,2])T(0,1)

    tau_y[k,1:2] <- 1/(se_prop[k,1:2]^2)
    }

## Priors
  for(s in 1:2) { # cross method variance (within a country)
    tau_alpha[s] ~ dt(0,1,1)T(0,)
  }
          }", file=paste0(main_path,"/model.txt"))
      }
    }
  }
}
