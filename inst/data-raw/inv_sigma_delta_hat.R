# Get variance-covariance matrix (sigma_delta) of the spline coefficient parameters for the local model

# Step 1:
# First run a multi-country model (using the `run_model` function) and save the results to a .RDS file

# Step 2: Pull out and save variance-covariance matrix for use in single-country model.
mod <- readRDS("mod_global_subnational.RDS") # Read in multi-country MCMC results
inv.sigma_delta_hat_subnationalmod <- mod$BUGSoutput$median$inv.sigma_delta
usethis::use_data(inv.sigma_delta_hat_subnationalmod, overwrite=TRUE)
