#' Get the r and z ratio point estimates from the separate chains of the JAGS model runs
#' R and Z are the intermediate parameters that are used to estimates the final proportions. See the model file for context.
#' @name get_subnational_P_point_estimates
#' @param main_path String. Path where you have set your model results to be saved to.
#' @param pkg_data Output of the `mcmsupplylocal::get_subnational_modelinputs()` function.
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param spatial TRUE/FALSE. Default is FALSE. spatial=FALSE retrieves the data for all subnational provinces across all countries without GPS information. spatial=TRUE retrieves for data for countries with GPS information as well as FP source data.
#' @param mycountry The country name of interest in a local run. You must have local=TRUE for this functionality. A list of possible countries available found in data/mycountries.rda.
#' @return returns the point estimates for the jags model object
#' @import R2jags runjags tidyverse tidybayes foreach doMC sf spdep geodata
#' @export

get_subnational_P_point_estimates <- function(main_path, pkg_data, local=FALSE, spatial=FALSE, mycountry=NULL) {

  # Get model inputs for cross matching to estimates
  n_country <- pkg_data$n_country
  n_subnat <- pkg_data$n_subnat
  n_method <- pkg_data$n_method
  n_sector <- c("Public", "Commercial_medical", "Other")
  n_all_years <- pkg_data$n_years
  mydata <- pkg_data$data
  all_years <- pkg_data$all_years
  K <- pkg_data$K
  B.ik <- pkg_data$B.ik

  # Creating index tables for reference ------------------------------------------------------------
  subnat_index_table <- mydata %>% select(Country, Region, index_subnat) %>% ungroup() %>% distinct()
  glimpse(subnat_index_table)

  country_index_table <- tibble(Country = n_country, index_country = unique(mydata$index_country))
  glimpse(country_index_table)

  method_index_table <- tibble(Method = n_method, index_method = 1:5)
  glimpse(method_index_table)

  sector_index_table <- tibble(Sector = n_sector, index_sector = 1:3)

  year_index_table <- tibble(average_year = all_years,
                             index_year = 1:n_all_years,
                             floored_year = floor(all_years))

  if(local==FALSE) { # global model estimates

    # Get ratio estimates. Saves to main_path folder pathway.
    mcmsupplylocal::get_subnational_r_z_samples(main_path, n_subnat, n_method, n_sector, n_all_years, K, B.ik, local=FALSE, spatial=FALSE)

    # Calculate proportions using the full posterior sample. Reads in the r and z variables using the main_path folder.
    mcmsupplylocal::get_subnational_global_P_samps(main_path)

    # Get point estimates for median, 95% and 80% credible intervals
    all_p_pub <- mcmsupplylocal::get_subnational_global_P_estimates(main_path, "P_public.RDS", subnat_index_table, method_index_table, "Public", year_index_table)
    all_p_CM <- mcmsupplylocal::get_subnational_global_P_estimates(main_path, "P_CM.RDS", subnat_index_table, method_index_table, "Commercial_medical", year_index_table)
    all_p_other <- mcmsupplylocal::get_subnational_global_P_estimates(main_path, "P_other.RDS", subnat_index_table, method_index_table, "Other", year_index_table)

    all_p <- rbind(all_p_pub, all_p_CM)
    all_p <- rbind(all_p, all_p_other)
  } else {
    # Read in chains results
    chain1 <- readRDS(paste0(main_path,"1chain.rds"))
    chain1 <- chain1$BUGSoutput$sims.matrix %>% as_tibble()

    chain2 <- readRDS(paste0(main_path,"2chain.rds"))
    chain2 <- chain2$BUGSoutput$sims.matrix %>% as_tibble()

    # Pull out P posterior samples for each of the three sectors
    public_samps <- dplyr::bind_rows(chain1[,stringr::str_detect(colnames(chain1), "P\\[1,")], chain2[,stringr::str_detect(colnames(chain2), "P\\[1,")])
    CM_samps <- dplyr::bind_rows(chain1[,stringr::str_detect(colnames(chain1), "P\\[2,")], chain2[,stringr::str_detect(colnames(chain2), "P\\[2,")])
    other_samps <- dplyr::bind_rows(chain1[,stringr::str_detect(colnames(chain1), "P\\[3,")], chain2[,stringr::str_detect(colnames(chain2), "P\\[3,")])

    # Get point estimates for median, 95% and 80% credible intervals
    pub_df <- mcmsupplylocal::get_subnational_local_P_estimates(public_samps, colnames(public_samps), subnat_index_table, method_index_table, sector_index_table, year_index_table)
    CM_df <- mcmsupplylocal::get_subnational_local_P_estimates(CM_samps, colnames(CM_samps), subnat_index_table, method_index_table, sector_index_table, year_index_table)
    other_df <- mcmsupplylocal::get_subnational_local_P_estimates(other_samps, colnames(other_samps), subnat_index_table, method_index_table, sector_index_table, year_index_table)

    all_p <- bind_rows(pub_df, CM_df, other_df)
  }

  if(is.null(mycountry)==TRUE) {
    saveRDS(all_p, file=paste0(main_path,"P_point_estimates.RDS"))
  } else {
    saveRDS(all_p, file=paste0(main_path,mycountry,"_P_point_estimates.RDS"))
  }
  return(all_p)
}


