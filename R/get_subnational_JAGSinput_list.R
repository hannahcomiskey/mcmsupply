#' Combines the data sources to create one JAGS input list
#' @name get_subnational_JAGSinput_list
#' @param pkg_data The data list from the 'mcmsupply::get_national_modelinputs' function.
#' @param local TRUE/FALSE. Default is FALSE for global runs. Decides if this is a single-country or global run.
#' @param spatial TRUE/FALSE. Default is FALSE. spatial=FALSE retrieves the data for all subnational provinces across all countries without GPS information. spatial=TRUE retrieves for data for countries with GPS information as well as FP source data.
#' @param mycountry The name of country of interest. Default is NULL. For the names of potential countries, review vignette.
#' @return returns a list ready for input into the JAGS model
#' @export

get_subnational_JAGSinput_list <- function(pkg_data, local= FALSE, spatial=FALSE,  mycountry=NULL) {
  if(local==TRUE & is.null(mycountry)==FALSE) {
    if(spatial==TRUE) { # local spatial
      local_params <- mcmsupply::get_subnational_local_parameters(mycountry=mycountry) # get local informative prior parameters
      geo_data <- mcmsupply::get_subnational_local_geodata(mycountry=mycountry) # get geographic neighbourhood adjacency matrix
      jags_data <- list(y = pkg_data$data[,c("Public", "Commercial_medical")], # create JAGS list
                        se_prop = pkg_data$data[,c("Public.SE", "Commercial_medical.SE")],
                        alpha_cms_hat = local_params$alpha_cms,
                        tau_alpha_snms_hat = local_params$tau_alphapms,
                        natRmat = local_params$natRmat,
                        natdf = length(pkg_data$n_method)+1,
                        kstar = pkg_data$kstar,
                        B.ik = pkg_data$B.ik,
                        n_years = pkg_data$n_years,
                        n_obs = pkg_data$n_obs,
                        K = pkg_data$K,
                        H = pkg_data$H,
                        W = geo_data$W,
                        D = geo_data$D,
                        P_zeroes = rep(0, length(pkg_data$n_subnat)),
                        P_count = pkg_data$P_count,
                        M_count = pkg_data$M_count,
                        matchsubnat = pkg_data$matchsubnat,
                        matchmethod = pkg_data$matchmethod,
                        matchyears = pkg_data$matchyears
      )
    } else { # local nonspatial
      local_params <- mcmsupply::get_subnational_local_parameters(mycountry = mycountry) # local informative prior parameters
      jags_data <- list(y = pkg_data$data[,c("Public", "Commercial_medical")], # create JAGS list
                        se_prop = pkg_data$data[,c("Public.SE", "Commercial_medical.SE")],
                        alpha_cms_hat = local_params$alpha_cms,
                        tau_alpha_snms_hat = local_params$tau_alphapms,
                        natRmat = local_params$natRmat,
                        natdf = length(pkg_data$n_method)+1,
                        kstar = pkg_data$kstar,
                        B.ik = pkg_data$B.ik,
                        n_years = pkg_data$n_years,
                        n_obs = pkg_data$n_obs,
                        K = pkg_data$K,
                        H = pkg_data$H,
                        P_zeroes = rep(0, length(pkg_data$n_subnat)),
                        P_count = pkg_data$P_count,
                        M_count = pkg_data$M_count,
                        matchsubnat = pkg_data$matchsubnat,
                        matchmethod = pkg_data$matchmethod,
                        matchyears = pkg_data$matchyears
      )
    }
  } else {
    if(spatial==TRUE) { # global spatial
      load("data/estimated_global_subnational_correlations.rda") # load global subnational correlations
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
                        W = pkg_data$W, # global spatial parameters
                        D = pkg_data$D, # global spatial parameters
                        P_zeroes = pkg_data$P_zeroes,
                        C_count = pkg_data$C_count,
                        P_count = pkg_data$P_count,
                        R_count = pkg_data$R_count,
                        M_count = pkg_data$M_count,
                        matchsubnat = pkg_data$matchsubnat,
                        matchcountry = pkg_data$matchcountry,
                        matchregion = pkg_data$matchsuperregion,
                        matchmethod = pkg_data$matchmethod,
                        matchyears = pkg_data$matchyears
      )
    } else { # global nonspatial
      load("data/estimated_global_subnational_correlations.rda") # load global subnational correlations
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
                        matchyears = pkg_data$matchyears
      )
    }
  }
  return(jags_data)
}
