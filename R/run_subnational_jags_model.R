#' Run the jags model for estimating the proportion of modern contraceptive methods supplied by the public & private sectors at a subnational level using a Bayesian hierarchical penalized spline model
#' @name run_subnational_jags_model
#' @param jagsdata The inputs for the JAGS model
#' @param jagsparams The parameters of the JAGS model you wish to review
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param spatial TRUE/FALSE. Default is FALSE. spatial=FALSE retrieves the data for all subnational provinces across all countries without GPS information. spatial=TRUE retrieves for data for countries with GPS information as well as FP source data.
#' @param main_path Default is "results/". String to indicate where to save results.
#' @param n_iter Default is 80000. Number of itterations to do in JAGS model.
#' @param n_burnin Default is 10000. Number of samples to burn-in in JAGS model.
#' @param n_thin Default is 35. Number of samples to thin by in JAGS model.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @return returns the jags model object
#' importFrom("stats", "cor", "filter", "lag")
#' @import R2jags runjags tidyverse tidybayes foreach doMC sf spdep geodata
#' @export

run_subnational_jags_model <- function(jagsdata, jagsparams, local=FALSE, spatial=FALSE, main_path = "results/",
                                       n_iter = 80000, n_burnin = 10000, n_thin = 35, mycountry=NULL) {
  doMC::registerDoMC() # start parallel runs, save results in steps

  # get model file
  modfile <- ifelse(local==FALSE & spatial==FALSE, "model/global_subnational_nonspatial_model.txt", # global non-spatial model
                    ifelse(local==FALSE & spatial==TRUE, "model/global_subnational_spatial_model.txt", # global spatial model
                           ifelse(local==TRUE & spatial==FALSE,"model/local_subnational_nonspatial_model.txt", # local non spatial model,
                                  "model/local_subnational_spatial_model.txt"))) # local spatial model
  n_chains = 2

  foreach(chain=1:n_chains) %dopar% {   ## Do chains separately ------------------------------
    set.seed(chain*1239)
    mod <- R2jags::jags(data = jagsdata,
                        parameters.to.save = jagsparams,
                        model.file = modfile,
                        n.chains = 1,
                        n.burnin = n_burnin,
                        n.iter = n_iter,
                        n.thin = n_thin,
                        working.directory = getwd()
    )
    ########### REMOVING SOME DATA FROM FIT OBJECT TO REDUCE FILE SIZE ####################################

    mod[names(mod) %in% c("model")] <- NA
    mod$BUGSoutput[names(mod$BUGSoutput) %in% c("sims.list",
                                                "summary",
                                                "mean",
                                                "sd")] <- NA
    saveRDS(mod, paste0(main_path,chain, "chain.rds"))
    print(paste("MCMC results for chain ", chain, "complete"))
  } # end chains

  gc()
  chain=1
  mod <- readRDS(paste0(main_path,chain, "chain.rds"))
  for (chain in 2:n_chains) {
    mod_for_one_chain <- readRDS(paste0(main_path,chain, "chain.rds"))
    mod$BUGSoutput$sims.array <- mod$BUGSoutput$sims.array %>% abind::abind(mod_for_one_chain$BUGSoutput$sims.array, along = 2)
  }

  # now we need to hack the fit object such that it has correct meta data (as the original git object had meta data for just 1 chain)
  mod$BUGSoutput$n.chains <- n_chains

  if(local==TRUE) { # save local subnational models
    if(spatial==TRUE) { saveRDS(mod, paste0(main_path, "mod_local_subnational_",mycountry,"_spatial.RDS")) } # spatial results
      else { saveRDS(mod, paste0(main_path, "mod_local_subnational_",mycountry,".RDS"))} # non-spatial model
  }
  else { # save global subnational models
    if(spatial==TRUE) { saveRDS(mod, paste0(main_path, "mod_global_subnational_spatial.RDS"))} # spatial results
      else { saveRDS(mod, paste0(main_path, "mod_global_subnational.RDS")) } # non-spatial model
    }
  return(mod)
}
