#' Function to pull the complete posterior sample for the national method-supply share estimates. Functionality for the subnational models is still under development.
#' @name get_posterior_P_samps
#' @param jagsdata The inputs for the JAGS model
#' @param model_output The output of the mcmsupply::run_jags_model() function.
#' @param  nposterior The number of posterior samples you wish to pull.
#' @return A dataframe containing the posterior samples of national method-supply share estimates.
#' @examples
#' \dontrun{
#' raw_data <- get_data(national=TRUE, local=TRUE, mycountry="Nepal")
#' jagsdata <- get_modelinputs(startyear=1990, endyear=2025.5, nsegments=12, raw_data)
#' mod <- run_jags_model(jagsdata = jagsdata, jagsparams = NULL, n_iter = 5, n_burnin = 1, n_thin = 1)
#' post_samps <- get_posterior_P_samps(jagsdata = jagsdata, model_output = mod, nposterior=4)
#' }
#' @export

get_posterior_P_samps <- function(jagsdata, model_output, nposterior){

  FP_source_data_wide <- jagsdata$modelinputs$data
  all_years <- jagsdata$modelinputs$all_years
  n_all_years <- jagsdata$modelinputs$n_years

  # Creating index tables for reference ------------------------------------------------------------
  country_index_table <- FP_source_data_wide %>% dplyr::ungroup() %>% dplyr::select(Country, index_country) %>% dplyr::distinct()
  region_index_table <- FP_source_data_wide %>% dplyr::ungroup() %>% dplyr::select(Super_region, index_superregion) %>% dplyr::distinct()
  method_index_table <- FP_source_data_wide %>% dplyr::ungroup() %>% dplyr::select(Method, index_method) %>% dplyr::distinct()
  sector_index_table <- tibble::tibble(Sector = c("Public", "Commercial_medical", "Other"), index_sector = 1:3)
  year_index_table <- tibble::tibble(average_year = all_years, index_year = 1:n_all_years, floored_year = floor(all_years))

  # Apply dimnames to P samps ------------------------------
  posterior_samps <- matrix()


  if(jagsdata$args$national==TRUE) {
    P_samps <- model_output$JAGS$BUGSoutput$sims.list$P
    dimnames(P_samps)[[2]] <- c("Public", "Commercial_medical", "Other")
    dimnames(P_samps)[[3]] <- jagsdata$modelinputs$n_method

    if(length(jagsdata$modelinputs$n_country)>1) { # National multi-country model results
    dimnames(P_samps)[[4]] <- jagsdata$modelinputs$n_country
    dimnames(P_samps)[[5]] <- 1:n_all_years
    } else { # National single-country model results
      dimnames(P_samps)[[4]] <- 1:n_all_years
    }
  } else {
    message("ERROR: Functionality for subnational model estimates remains under development.")
    stop()
  }
    # if(jagsdata$args$national==FALSE){ FUTURE WORK AT FUNCTIONALITY FOR SUBNATIONAL
    #   subnat_p <- mcmsupply::get_subnational_P_point_estimates(pkg_data = jagsdata, local=jagsdata$args$local, mycountry=jagsdata$args$mycountry, n_chain=2)
    #   if(length(jagsdata$modelinputs$n_country)>1) { # Subnational multi-country model results
    #     dimnames(P_samps)[[4]] <- jagsdata$modelinputs$n_subnat
    #     dimnames(P_samps)[[5]] <- 1:n_all_years
    #   } else {
    #     # Subnational single-country model results
    #     if(jagsdata$args$national==FALSE & length(jagsdata$modelinputs$n_country)==1) {
    #       dimnames(P_samps)[[4]] <- jagsdata$modelinputs$n_country
    #       dimnames(P_samps)[[5]] <- 1:n_all_years
    #     }
    #   }
    # }

  # Data manipulation to pull out posterior sample
  if(length(jagsdata$modelinputs$n_country)>1) {  # National multi-country model results
    for(c in jagsdata$modelinputs$n_country) {
      P_samps_tmp <- plyr::adply(P_samps[,,,c,], c(2,3,4)) # flatten array to matrix
      samp_tot <- ncol(P_samps_tmp)-3 # gives number of posterior samples
      colnames(P_samps_tmp) <- c("sector_category", "Method", "index_year", c(1:samp_tot))
      P_samps_tmp <- P_samps_tmp %>%
        tidyr::pivot_longer(cols=c(4:ncol(P_samps_tmp)), names_to="sample_id", values_to = "p_samp") %>%
        dplyr::mutate(Country=c) %>%
        dplyr::mutate(index_year = as.numeric(index_year)) %>%
        dplyr::left_join(year_index_table) %>%
        dplyr::filter(sample_id < nposterior+1 & average_year > floored_year) %>% # filter to include up to samples of size 'nposterior'
        dplyr::select(sample_id, Country, Method, sector_category, p_samp, average_year)
      c_count <- which(jagsdata$modelinputs$n_country==c) # to begin matrix of combined results
      if(c_count == 1) {
        posterior_samps <- P_samps_tmp
      } else {
        posterior_samps <- rbind(posterior_samps, P_samps_tmp)
      }
    }
  } else { # National single-country model results
    P_samps_tmp <- plyr::adply(P_samps, c(2,3,4)) # flatten array to matrix
    samp_tot <- ncol(P_samps_tmp)-3 # gives number of posterior samples
    colnames(P_samps_tmp) <- c("sector_category", "Method", "index_year", c(1:samp_tot))
    posterior_samps <- P_samps_tmp %>%
      tidyr::pivot_longer(cols=c(4:ncol(P_samps_tmp)), names_to="sample_id", values_to = "p_samp") %>%
      dplyr::mutate(Country= jagsdata$args$mycountry) %>%
      dplyr::mutate(index_year = as.numeric(index_year)) %>%
      dplyr::left_join(year_index_table) %>%
      dplyr::filter(sample_id < nposterior+1 & average_year > floored_year) %>% # filter to include up to samples of size 'nposterior'
      dplyr::select(sample_id, Country, Method, sector_category, p_samp, average_year)
  }

  return(posterior_samps)
}

