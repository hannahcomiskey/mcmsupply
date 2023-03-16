#' Get the r and z ratio point estimates from the separate chains of the JAGS model runs
#' R and Z are the intermediate parameters that are used to estimates the final proportions. See the model file for context.
#' @name get_subnational_r_z_samples
#' @param main_path String. Path where you have set your model results to be saved to.
#' @param pkg_data Output of the `mcmsupplylocal::get_subnational_modelinputs()` function.
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param spatial TRUE/FALSE. Default is FALSE. spatial=FALSE retrieves the data for all subnational provinces across all countries without GPS information. spatial=TRUE retrieves for data for countries with GPS information as well as FP source data.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @return returns the point estimates for the jags model object
#' @import R2jags runjags tidyverse tidybayes foreach doMC sf spdep geodata
#' @export

get_subnational_r_z_samples <- function(main_path,  n_subnat, n_method, n_sector, n_all_years, K, B.ik, local=FALSE, spatial=FALSE) {

  chain1 <- readRDS(paste0(main_path,"1chain.rds"))
  chain1 <- chain1$BUGSoutput$sims.matrix %>% as_tibble()

  chain2 <- readRDS(paste0(main_path,"2chain.rds"))
  chain2 <- chain2$BUGSoutput$sims.matrix %>% as_tibble()

  # Create P estimates --------------------------------------
  n_samps <- 2000
  P_count = length(n_subnat)
  M_count = length(n_method)
  S_count = length(n_sector)

  z <- r <- array(dim = c(2*n_samps, M_count, P_count, n_all_years))

  for(m in 1:M_count){ # method loop
    for(p in 1:P_count) { # province loop matched to C
      for (t in 1:n_all_years) {
        # get column names
        alphapub_param <- paste0("alpha_pms[",1,",",m,",",p,"]")
        alphapriv_param <- paste0("alpha_pms[",2,",",m,",",p,"]")

        betakpub_param <- paste0("beta.k[",1,",",m,",",p,",",paste0(1:K,"]"))
        betakpriv_param <- paste0("beta.k[",2,",",m,",",p,",",paste0(1:K,"]"))

        phipub_param <- paste0("phi[1,",p,"]")
        phipriv_param <- paste0("phi[2,",p,"]")

        # chain 1 public and private
        alpha_sampspub1 <- chain1[,which(colnames(chain1)==alphapub_param)] %>% unlist() %>% as.vector()
        beta_sampspub1 <- chain1[,which(colnames(chain1) %in% betakpub_param)] %>% as.matrix()
        alpha_sampspriv1 <- chain1[,which(colnames(chain1)==alphapriv_param)] %>% unlist() %>% as.vector()
        beta_sampspriv1 <- chain1[,which(colnames(chain1) %in% betakpriv_param)] %>% as.matrix()

        # chain 2 public and private
        alpha_sampspub2 <- chain2[,which(colnames(chain2)==alphapub_param)] %>% unlist() %>% as.vector()
        beta_sampspub2 <- chain2[,which(colnames(chain2) %in% betakpub_param)] %>% as.matrix()
        alpha_sampspriv2 <- chain2[,which(colnames(chain2)==alphapriv_param)] %>% unlist() %>% as.vector()
        beta_sampspriv2 <- chain2[,which(colnames(chain2) %in% betakpriv_param)] %>% as.matrix()

        if(spatial==TRUE) {
          phi_sampspub1 <- chain1[,which(colnames(chain1) %in% phipub_param)] %>% as.matrix() # spatial parameter
          phi_sampspriv1 <- chain1[,which(colnames(chain1) %in% phipriv_param)] %>% as.matrix()
          phi_sampspub2 <- chain2[,which(colnames(chain2) %in% phipub_param)] %>% as.matrix()
          phi_sampspriv2 <- chain2[,which(colnames(chain2) %in% phipriv_param)] %>% as.matrix()
          z[1:n_samps,m,p,t] <- alpha_sampspub1 + (B.ik[p,t,] %*% t(beta_sampspub1)) + t(phi_sampspub1) # public sector
          z[(n_samps+1):(2*n_samps),m,p,t] <- alpha_sampspub2 + (B.ik[p,t,] %*% t(beta_sampspub2)) + t(phi_sampspub2)
          r[1:n_samps,m,p,t] <- alpha_sampspriv1 + (B.ik[p,t,] %*% t(beta_sampspriv1)) + t(phi_sampspriv1) # private sector
          r[(n_samps+1):(2*n_samps),m,p,t] <- alpha_sampspriv2 + (B.ik[p,t,] %*% t(beta_sampspriv2)) + t(phi_sampspriv2)
          }
        else {
            z[1:n_samps,m,p,t] <- alpha_sampspub1 + (B.ik[p,t,] %*% t(beta_sampspub1))
            z[(n_samps+1):(2*n_samps),m,p,t] <- alpha_sampspub2 + (B.ik[p,t,] %*% t(beta_sampspub2))
            # private sector
            r[1:n_samps,m,p,t] <- alpha_sampspriv1 + (B.ik[p,t,] %*% t(beta_sampspriv1))
            r[(n_samps+1):(2*n_samps),m,p,t] <- alpha_sampspriv2 + (B.ik[p,t,] %*% t(beta_sampspriv2))
        }
      } # end time loop
    } # end M loop
  } # end P loop

  saveRDS(z, paste0(main_path,"zsamps.RDS"))
  saveRDS(r, paste0(main_path,"rsamps.RDS"))

  return(list(r = r,
              z = z))
}


