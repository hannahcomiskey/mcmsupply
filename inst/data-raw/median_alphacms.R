# Get median estimate of the country-level (alpha_cms) intercept parameter for the local model

respath = "results/global_nonspatial/" # Edit to match your saved results path location.

mod <- readRDS(paste0(respath,"mod_global_subnational.RDS")) # Read in global model from your designated results folder.
# Can be either spatial or non-spatial ideally depending on what kind of local model you are running.
# However, the estimated intercepts are not affected by the presence of a spatial component in the model.

alpha_cms_hat <- mod$BUGSoutput$median$alpha_cms
saveRDS(alpha_cms_hat, file=paste0(respath,"alpha_cms_hat.RDS"))
