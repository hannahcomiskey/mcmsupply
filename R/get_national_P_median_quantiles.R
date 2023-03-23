#' Get median and 95% credible interval for posterior samples of P from national JAGS model
#' @name get_national_P_median_quantiles
#' @param country_index_table Dataframe with country indexing applied. Used to match estimates to data.
#' @param method_index_table Dataframe with method indexing applied. Used to match estimates to data.
#' @param sector_index_table Dataframe with sector indexing applied. Used to match estimates to data.
#' @param year_index_table Dataframe with time indexing applied. Used to match estimates to data.
#' @param my_model JAGS model
#' @return Dataframe of labelled posterior samples with median and 95% credible intervals estimates.
#' @export
#'
# Calculation for sector data
get_national_P_median_quantiles <- function(country_index_table, method_index_table, sector_index_table, year_index_table, my_model, local=FALSE) { # Median alpha values
  years <- unique(year_index_table$floored_year)
  n_years <- years %>% length() # important when using 6-monthly description
  n_all_years <- nrow(year_index_table)
  if(local==FALSE) {
    P_samp <- my_model$BUGSoutput$sims.list$P
    P_dims <- dim(P_samp)
    print(P_dims)

    P_s_med <- array(dim=c(P_dims[4],n_years,P_dims[3],P_dims[2])) # 30x5 matrix for CountryxMethod then 5 into an array for each sector

    # Create a table for storing individual true country public data
    for(k in 1:n_years) { # time loop
      year <- years[k]
      time_index <- year_index_table %>% # match index years to pooled years (pool 6 monthly estimates)
        dplyr::filter(floored_year == year) %>%
        dplyr::select(index_year) %>%
        unlist() %>%
        as.vector()
      for(j in 1:P_dims[4]) { # country
        for (r in 1:P_dims[3]) { # method
          for (i in 1:P_dims[2]) { # sector
            P_s_med[j,k,r,i] <- median(P_samp[,i,r,j,time_index])
          }
        }
      }
    }

    P_s_med <- plyr::adply(P_s_med, c(1,2,3,4))
    colnames(P_s_med) <- c("index_country", "index_year", "index_method", "index_sector", "median_p")

    P_s_med <- P_s_med %>% mutate(
      index_country = as.numeric(index_country),
      index_method = as.numeric(index_method),
      index_sector = as.numeric(index_sector),
      index_year = as.numeric(index_year)
    )

    upper_P_s_med <- lower_P_s_med <- array(dim=c(P_dims[4],n_years,P_dims[3],P_dims[2])) # 30x5 matrix for CountryxMethod then 5 into an array for each sector

    # Create a table for storing individual true country public data
    for(t in 1:n_years) { # time loop
      year <- years[t]
      time_index <- year_index_table %>% # match index years to pooled years (pool 6 monthly estimates)
        dplyr::filter(floored_year == year) %>%
        dplyr::select(index_year) %>%
        unlist() %>%
        as.vector()

      for(j in 1:P_dims[4]) {
        for (r in 1:P_dims[3]) { # Create a table for storing individual true country public data
          for (i in 1:P_dims[2]) {
            upper_P_s_med[j,t,r,i] <- quantile(P_samp[,i,r,j,time_index], probs = 0.975, na.rm=TRUE) # upper quantile using the whole year
          }
        }
      }
    }

    upper_P_s_med <- plyr::adply(upper_P_s_med, c(1,2,3,4))
    colnames(upper_P_s_med) <- c("index_country", "index_year", "index_method", "index_sector", "upper_95")

    for(t in 1:n_years) { # time loop
      year <- years[t]
      time_index <- year_index_table %>% # match index years to pooled years (pool 6 monthly estimates)
        dplyr::filter(floored_year == year) %>%
        dplyr::select(index_year) %>%
        unlist() %>%
        as.vector()
      for(j in 1:P_dims[4]) { # country loop
        for (r in 1:P_dims[3]) { # method loop
          for (i in 1:P_dims[2]) { # sector loop
            lower_P_s_med[j,t,r,i] <- quantile(P_samp[,i,r,j,time_index], probs = 0.025) # lower quantile using the whole year
          }
        }
      }
    }

    lower_P_s_med <- plyr::adply(lower_P_s_med, c(1,2,3,4)) # changes shape of array to matrix
    colnames(lower_P_s_med) <- c("index_country", "index_year", "index_method", "index_sector", "lower_95")

    P_s_Q <- merge(lower_P_s_med, upper_P_s_med)

    P_s_Q <- P_s_Q %>% mutate(
      index_country = as.numeric(index_country),
      index_method = as.numeric(index_method),
      index_sector = as.numeric(index_sector),
      index_year = as.numeric(index_year)
    )


    P_s_med <- P_s_med %>%
      dplyr::left_join(P_s_Q)

    # Match mid-year indexing to model posterior samples
    whole_years <- year_index_table %>%
      dplyr::select(floored_year) %>%
      unlist() %>%
      as.vector() %>%
      unique()
    time_tb <- tibble::tibble(index_year = 1: length(whole_years), average_year = whole_years + 0.5)

    P_s_med <- P_s_med %>%
      dplyr::left_join(country_index_table) %>% # match strings to numeric indexing
      dplyr::left_join(time_tb) %>%
      dplyr::left_join(method_index_table) %>%
      dplyr::left_join(sector_index_table) %>%
      dplyr::group_by(Country, Method, Sector, average_year) %>%
      dplyr::select(Country, Method, Sector, average_year, median_p, lower_95, upper_95) %>%
      dplyr::distinct() %>%
      dplyr::rowwise() %>%
      dplyr::filter(average_year > floor(average_year)) # only take the mid-years

  } else {
    mycountry <- unique(country_index_table$Country)
    m <- my_model$BUGSoutput$sims.matrix # create an object containing the posterior samples
    sample_draws <- tidy_draws(m)     ## format data for plotting results
    n_iter <- nrow(sample_draws)
    P_start <- "P[1,1,1]"
    P_end <- paste0("P[",nrow(sector_index_table),",",nrow(method_index_table),",",nrow(year_index_table),"]")
    col1 <- which(colnames(sample_draws)==P_start)
    col2 <- which(colnames(sample_draws)==P_end)
    tnp_samp <- sample_draws[,c(col1:col2)]
    P_s_med <- tnp_samp %>%
      tidyr::pivot_longer(cols = everything(),
                          names_to = "params",
                          values_to = "mu_pred"
      ) %>%
      dplyr::mutate(index_sector = as.numeric(substr(params, nrow(sector_index_table),nrow(sector_index_table)))) %>%
      dplyr::mutate(index_method = as.numeric(substr(params, nrow(method_index_table),nrow(method_index_table)))) %>%
      dplyr::mutate(index_year = rep(rep(1:n_all_years, each = nrow(sector_index_table)*nrow(method_index_table)), n_iter)) %>%
      dplyr::left_join(year_index_table) %>%
      dplyr::left_join(method_index_table) %>%
      dplyr::left_join(sector_index_table) %>%
      dplyr::select(mu_pred, average_year, Method, Sector) %>%
      dplyr::group_by(Method, Sector, average_year) %>%
      dplyr::mutate(median_p = median(mu_pred),
                    lower_95 = quantile(mu_pred, prob = 0.025),
                    upper_95 = quantile(mu_pred, prob = 0.975))%>%
      dplyr::mutate(Country = mycountry) %>%
      dplyr::select(Country, Method, Sector, average_year, median_p, lower_95, upper_95) %>%
      dplyr::distinct()
    }
  return(P_s_med)
}
