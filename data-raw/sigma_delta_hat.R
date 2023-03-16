# Get variance-covariance matrix (sigma_delta) of the spline coefficient parameters for the local model

respath = "results/global_nonspatial/" # Edit to match your saved results path location.

mod <- readRDS(paste0(respath,"mod_global_subnational.RDS")) # Read in global model from your designated results folder. Edit to match your path location.
# Can be either spatial or non-spatial ideally depending on what kind of local model you are running.

sigma_delta_hat <- mod$BUGSoutput$median$sigma_delta
saveRDS(sigma_delta_hat, file=paste0(respath,"sigma_delta_hat.RDS"))
