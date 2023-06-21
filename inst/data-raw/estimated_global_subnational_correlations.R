# Source code --------------------------------------
pkg_data <- mcmsupplylocal::get_subnational_modelinputs(fp2030=TRUE, local=FALSE,
                                                        spatial=FALSE, mycountry=NULL,
                                                        startyear=1990, endyear=2028.5,
                                                        nsegments=12, raw_subnatdata)

repath = "results/global_subnat/0covariance/" # Set path to where you save your model results
vispath = "visualisation/global_estimation/correlations/" # Set path to where you save your visualisations

mod <- readRDS(paste0(repath, "ref_alpha_Bspline_0covar_globalsubnat_mod.RDS")) # Read in 0-covariance model

post_samps <-mod$BUGSoutput$sims.array

# Create delta.k estimates --------------------------------------
FP_source_data_wide <- pkg_data$data
n_samps <- 2000
P_count = pkg_data$P_count
M_count = pkg_data$M_count
S_count = 3

B.ik <- pkg_data$B.ik[,,1]

# Pull out year index of splines
splines_year <- apply(B.ik, 2, function(x) which(x!=0)) # where spline is non-0... All splines are the same for method and countries
dim(knots.all) # pulled from set_up_jags_SE_data_refalpha script... Knot locations vary by country and method

# Get FOD spline coefficients
delta_estimates <- mod$BUGSoutput$median$delta.k

# Approach 2: match index_year to between active splines
for(i in 1:nrow(FP_source_data_wide)) {
  index_year <- FP_source_data_wide$index_year[i]
  active_splines <- sapply(splines_year, function(e) is.element(index_year, e)) # compare index of year to index of active splines
  active_delta <- which(ifelse(active_splines==1,1,0)==1)-1 # H=K-1
  active_delta <- active_delta[active_delta>0]
  FP_source_data_wide$delta[i] <- I(list(active_delta))
}

# Pull out data years
data_index <- FP_source_data_wide %>%
  dplyr::ungroup() %>%
  dplyr::select(index_subnat, delta) %>%
  dplyr::distinct() # pulling out all methods

# Calculate R init --------------------------------------------
# Rbasic matrix - for each sector, 5x5 matrix
# creating data_index for list
subnat_vec <- vector()
for(i in 1:nrow(data_index)) {
  index_subnat <- rep(data_index$index_subnat[i], length(data_index$delta[[i]]))
  subnat_vec <- c(subnat_vec, index_subnat)
}

public_estimates <- delta_estimates[1,,subnat_vec,unlist(data_index$delta)]

public_R <- var_public_R <- matrix(NA, 5,5)
for(i in 1:5) {
  for(j in 1:5) {
    delta_1 <- public_estimates[i,,] # delta_i
    delta_2 <- public_estimates[j,,] # delta_j
    btm_frac <- sqrt(sum(delta_1^2)) * sqrt(sum(delta_2^2))
    top_frac <- sum(delta_1 * delta_2) # sum over product of countries and knots !! Check: over different countries
    public_R[i,j] <- top_frac/btm_frac
    n <- nrow(delta_1)
    var_public_R[i,j] <- (1-(public_R[i,j])^2)/(n-2) # variance of correlation
  }
}

var_cor_public <- diag(var_public_R)

# R_pd - incorporates some uncorrelated noise into calculation
# Do we need this? Used as original matrix is low rank. Do we think ours is?
# Swapping to public_R doesn't seem to have any effect in outcome

R_pub <- 0.99*public_R + 0.01*diag(x=1,5,5)
colnames(R_pub) <- rownames(R_pub) <- c("Female Sterilization","Implants", "Injectables",  "IUD","OC Pills")

# Commerical Med. to Total Private Ratio Estimation --------------------------------------------
# Rbasic matrix - for each sector, 5x5 matrix
TPR_estimates <- delta_estimates[2,,subnat_vec,unlist(data_index$delta)]

var_TPR_R <- TPR_R <- matrix(NA, 5,5)
for(i in 1:5) {
  for(j in 1:5) {
    delta_1 <- TPR_estimates[i,,] # delta_i
    delta_2 <- TPR_estimates[j,,] # delta_j
    btm_frac <- sqrt(sum(delta_1^2)) * sqrt(sum(delta_2^2))
    top_frac <- sum(delta_1 * delta_2) # sum over product of countries and knots
    TPR_R[i,j] <- top_frac/btm_frac
    n <- nrow(delta_1)
    var_TPR_R[i,j] <- (1-(TPR_R[i,j])^2)/(n-2) # variance of correlation
  }
}

var_cor_TPR <- diag(var_TPR_R) # get off diagonal elements
var_corr <- tibble(public = var_cor_public, TPR = var_cor_TPR)

# R_pd - incorporates some uncorrelated noise into calculation (why? Swapping to public_R doesn't seem to have any effect in outcome)

R_priv <- 0.99*TPR_R + 0.01*diag(x=1,5,5)
colnames(R_priv) <- rownames(R_priv) <- c("Female Sterilization","Implants", "Injectables",  "IUD","OC Pills")

# Set up correlations as per original set up -------------------------------------
my_public_cor_matrix <- mcmsupplylocal::flat_cor_mat(R_pub) %>%
  rename(public_cor = cor)

my_private_cor_matrix <- mcmsupplylocal::flat_cor_mat(R_priv) %>%
  rename(private_cor = cor)

# Creating correlation matrix for public:private

my_SE_rho_matrix <- merge(my_public_cor_matrix, my_private_cor_matrix, all = TRUE)

# Missing some pairs for correlation
correlation_pairs <- tribble(
  ~row ,  ~column,
  "Implants" , "Female Sterilization",
  "Injectables" , "Female Sterilization",
  "IUD" , "Female Sterilization",
  "OC Pills" , "Female Sterilization",
  "Injectables", "Implants" ,
  "IUD", "Implants" ,
  "OC Pills", "Implants" ,
  "IUD", "Injectables" ,
  "OC Pills", "Injectables" ,
  "OC Pills", "IUD"
)

test_rho_matrix <- dplyr::left_join(correlation_pairs, my_SE_rho_matrix)

saveRDS(test_rho_matrix, paste0(repath,"estimated_global_subnational_correlations.RDS")) # Save file with appropriate name depending on whether you are using a spatial model or not
#saveRDS(test_rho_matrix, paste0(repath,"estimated_global_subnational_spatial_correlations.RDS")) # Save file with appropriate name
