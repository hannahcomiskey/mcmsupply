# Get precision of the district-level (alpha_pms) intercept parameters for the local model

respath = "results/global_nonspatial/" # Edit to match your saved results path location.

mod <- readRDS(paste0(respath,"mod_global_subnational.RDS")) # Read in global model from your designated results folder. Edit to match your path location.
# Can be either spatial or non-spatial ideally depending on what kind of local model you are running.
# However, the estimated intercepts are not affected by the presence of a spatial component in the model.

tau_alpha_pms_hat <- mod$BUGSoutput$median$tau_alpha
saveRDS(tau_alpha_pms_hat, file=paste0(respath,"tau_alpha_pms_hat.RDS"))
