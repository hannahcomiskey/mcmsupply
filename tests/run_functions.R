devtools::load_all()
library(mcmsupply)

source_data <- mcmsupply::get_data(fp2030=TRUE)

cleanedsource_data <- mcmsupply::set_up_jags_data(source_data)

# Adding indexes to proportion -----------------------------------------------------
cleanedsource_data <- country_index_fun(cleanedsource_data, unique(cleanedsource_data$Country))
cleanedsource_data <- region_index_fun(cleanedsource_data, unique(cleanedsource_data$Region))
n_method <- c("Female Sterilization","Implants", "Injectables", "IUD","OC Pills")
cleanedsource_data <- method_index_fun(cleanedsource_data, n_method)

# Time indexing - important for splines -------------------------
all_years <- seq(from = 1990, to = 2025.5, by=0.5)
n_all_years <- length(all_years)

time_index_table <- tibble::tibble(average_year = all_years, index_year = 1:length(all_years))

cleanedsource_data <- cleanedsource_data %>%
  dplyr::mutate(index_year = match(average_year,all_years))

# Get T_star and match_Tstar -------------------------
T_star <- cleanedsource_data %>%
  dplyr::group_by(Country) %>%
  dplyr::filter(index_year==max(index_year)) %>%
  dplyr::select(Country, index_country, average_year, index_year) %>%
  dplyr::arrange(index_country) %>%
  dplyr::ungroup() %>%
  dplyr::select(index_country, index_year, average_year) %>%
  dplyr::distinct()

# Get inputs for JAGS model -------------------------
t_seq_2 <- floor(cleanedsource_data$index_year) # Time sequence for countries
country_seq <- cleanedsource_data$Country
n_country <- as.character(unique(country_seq)) # Names of countries
n_region <- unique(cleanedsource_data$Region) # Names of regions
n_sector <- c("Public", "Commercial_medical", "Other") # Names of categories
n_obs <- nrow(cleanedsource_data) # Total number of observations
year_seq <- seq(min(t_seq_2),max(t_seq_2), by=1) # Year sequence
n_years <- length(year_seq) # Number of years

# Find the indexes of observations that match to the predicted responses -------------------------
match_country <- cleanedsource_data$index_country
match_years <- cleanedsource_data$index_year
match_method <- cleanedsource_data$index_method
region_country <- cleanedsource_data %>%
  dplyr::select(Country, Region, index_country, index_region) %>%
  dplyr::distinct()
match_region <- region_country$index_region

# Get country-specific basis functions and related elements for JAGS model input ------------
nseg=12
Kstar <- vector()
B.ik <- array(dim = c(length(n_country), length(all_years),nseg+3))
knots.all <- matrix(nrow = length(n_country), ncol=nseg+3)
for(i in 1:nrow(T_star)) {
  index_mc <- T_star$average_year[i]
  res <- bs_bbase_precise(all_years, lastobs=index_mc, nseg = nseg) # number of splines based on segments, here choosing 10
  B.ik[i,,] <- res$B.ik
  Kstar[i] <- res$Kstar
  knots.all[i,] <- res$knots.k
}

K <- dim(res$B.ik)[2]
H <- K-1

# Correlations
load("data/estimated_rho_matrix.rda")
estimated_rho_matrix <- estimated_rho_matrix %>%
  dplyr::select(row, column, public_cor, private_cor)
my_SE_rho_matrix <- estimated_rho_matrix %>%
  dplyr::select(public_cor, private_cor)

# JAGS inputs ------------------
jagsdata <- list(y = cleanedsource_data[,c("Public", "Commercial_medical")],
                 se_prop = cleanedsource_data[,c("Public.SE", "Commercial_medical.SE")],
                 rho = my_SE_rho_matrix,
                 tstar = T_star$index_year,
                 kstar = Kstar,
                 B.ik = B.ik, #bbase function
                 n_years = n_all_years,
                 n_obs = n_obs,
                 K = K,
                 H = H,
                 R_count = length(n_region),
                 C_count = length(n_country),
                 M_count = length(n_method),
                 matchregion = match_region,
                 matchcountry = match_country,
                 matchmethod = match_method,
                 matchyears = match_years) # Inputs for the JAGS model

jags_pars <- c("P", "beta.k", "alpha_cms") # Parameters we wish to observe

# run jags model -------------------
mymod <- mcmsupply::run_jags_model(jagsdata, jags_pars)

# # tests -------------------
# org_mod <- readRDS("tests/testthat/ref_alpha_Bspline_15knots_6monthest.RDS")
# usethis::use_testthat()
# devtools::test()
