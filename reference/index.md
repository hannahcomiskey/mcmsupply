# Package index

## All functions

- [`Country_and_area_classification`](https://hannahcomiskey.github.io/mcmsupply/reference/Country_and_area_classification.md)
  : The Country and area classification according to the United Nations
  Standaistical Division, Standard country or area codes for statistical
  use (M49). Adapted for use in FP2030 by the Track20 project. A subset
  of data from the United Nations country classifications
- [`DEFT_DHS_database`](https://hannahcomiskey.github.io/mcmsupply/reference/DEFT_DHS_database.md)
  : DEFT_DHS_database A database of the design effects for some of the
  DHS surveys in the national and subnational datasets. Due to due to
  multistage and clustering of the DHS sample, the average standard
  error is increased by a design effect (DEFT) factor over that in an
  equivalent simple random sample.
- [`calculate_SE_data_from_DTA()`](https://hannahcomiskey.github.io/mcmsupply/reference/calculate_SE_data_from_DTA.md)
  : Calculate Standard Errors and Variance-Covariance Matrices for DHS
  Subnational Estimates
- [`check_data_freshness()`](https://hannahcomiskey.github.io/mcmsupply/reference/check_data_freshness.md)
  : Check Data Freshness Against DHS Reference
- [`country_names`](https://hannahcomiskey.github.io/mcmsupply/reference/country_names.md)
  : The names of the countries with national and subnational
  administration level data stored
- [`get_data()`](https://hannahcomiskey.github.io/mcmsupply/reference/get_data.md)
  : Wrapper function that retrieves the DHS data used for modelling the
  proportion of modern contraceptives supplied by the public and private
  sectors at the national and subnational administration levels.
- [`get_posterior_P_samps()`](https://hannahcomiskey.github.io/mcmsupply/reference/get_posterior_P_samps.md)
  : Function to pull the complete posterior sample for the national
  method-supply share estimates. Functionality for the subnational
  models is still under development.
- [`get_modelinputs()`](https://hannahcomiskey.github.io/mcmsupply/reference/get_subnational_modelinputs.md)
  : Get JAGS model inputs
- [`harvest_dhs()`](https://hannahcomiskey.github.io/mcmsupply/reference/harvest_dhs.md)
  : Harvest DHS Dataset Availability
- [`national_FPsource_VARCOV_bivarlogitnormal`](https://hannahcomiskey.github.io/mcmsupply/reference/national_FPsource_VARCOV_bivarlogitnormal.md)
  : An array of variance-covariance matrices transformed onto the
  logit-scale via the delta method. Each matrix corresponds to the DHS
  survey logit-transformed observations for the proportion of modern
  contraceptives supplied by the public and private sectors at the
  national level
- [`national_FPsource_data`](https://hannahcomiskey.github.io/mcmsupply/reference/national_FPsource_data.md)
  : DHS survey observations for the proportion of modern contraceptives
  supplied by the public and private sectors at the national level
- [`national_FPsource_format`](https://hannahcomiskey.github.io/mcmsupply/reference/national_FPsource_format.md)
  : A checklist for ensuring national-level custom data is appropriate
  to be used for estimation
- [`national_estimated_correlations_bivarlogitnormal`](https://hannahcomiskey.github.io/mcmsupply/reference/national_estimated_correlations_bivarlogitnormal.md)
  : The estimated national-level correlations between the rates of
  change in methods
- [`national_inv_sigma_delta_hat_bivarlogitnorm`](https://hannahcomiskey.github.io/mcmsupply/reference/national_inv_sigma_delta_hat_bivarlogitnorm.md)
  : The median estimate for the national-level variance-covariance
  matrix of the delta.k terms in the multi-country national model.
- [`national_tau_alpha_cms_hat_bivarlogitnorm`](https://hannahcomiskey.github.io/mcmsupply/reference/national_tau_alpha_cms_hat_bivarlogitnorm.md)
  : The median estimates of the precision for the national-level
  country, sector-, method-specific intercepts in the multi-country
  national model. This vector is used to inform the precision in the
  Normal prior of the national-level intercept in single-country
  national models.
- [`national_theta_rms_hat_bivarlogitnorm`](https://hannahcomiskey.github.io/mcmsupply/reference/national_theta_rms_hat_bivarlogitnorm.md)
  : The median estimates of the national-level sub-continental, sector-,
  method-specific intercepts in the multi-country national model. This
  array is used to inform the Normal prior of the country-level
  intercept in the single-country national model.
- [`national_varcov_order_bivarlogitnormal`](https://hannahcomiskey.github.io/mcmsupply/reference/national_varcov_order_bivarlogitnormal.md)
  : The order of observations to join the variance-covariance array data
  and the DHS survey observations for the proportion of modern
  contraceptives supplied by the public and private sectors at the
  national level
- [`plot_estimates()`](https://hannahcomiskey.github.io/mcmsupply/reference/plot_estimates.md)
  : Wrapper function to plot the JAGS estimates
- [`pull_estimates()`](https://hannahcomiskey.github.io/mcmsupply/reference/pull_estimates.md)
  : Function to pull method-supply share median estimates and credible
  intervals for a given year and country.
- [`run_jags_model()`](https://hannahcomiskey.github.io/mcmsupply/reference/run_jags_model.md)
  : Wrapper function to run the jags model for estimating the proportion
  of modern contraceptive methods supplied by the public & private
  Sectors using a Bayesian hierarchical penalized spline model for the
  national and subnational administration levels
- [`subnat_FPsource_data`](https://hannahcomiskey.github.io/mcmsupply/reference/subnat_FPsource_data.md)
  : DHS survey observations for the proportion of modern contraceptives
  supplied by the public and private sectors at the subnational
  administration level.
- [`subnat_FPsource_format`](https://hannahcomiskey.github.io/mcmsupply/reference/subnat_FPsource_format.md)
  : A checklist for ensuring subnational-level custom data is
  appropriate to be used for estimation
- [`subnational_alpha_cms_hat`](https://hannahcomiskey.github.io/mcmsupply/reference/subnational_alpha_cms_hat.md)
  : The median estimates of the subnational-level country, sector-,
  method-specific intercepts in the multi-country subnational model.
  This array is used to inform the Normal prior of the subnational-level
  intercept in the single-country subnational model.
- [`subnational_estimated_correlations`](https://hannahcomiskey.github.io/mcmsupply/reference/subnational_estimated_correlations.md)
  : The estimated subnational-level correlations between the rates of
  change in methods
- [`subnational_inv.sigma_delta_hat`](https://hannahcomiskey.github.io/mcmsupply/reference/subnational_inv.sigma_delta_hat.md)
  : The median estimate for the subnational-level precision matrix of
  the delta.k terms in the multi-country subnational model. This array
  is used to inform the multi-variate normal prior in the single-country
  subnational model.
- [`subnational_tau_alpha_pms_hat`](https://hannahcomiskey.github.io/mcmsupply/reference/subnational_tau_alpha_pms_hat.md)
  : subnational_tau_alpha_pms_hat The median estimates of the precision
  for the subnational-level country, sector-, method-specific intercepts
  in the multi-country subnational model. This vector is used to inform
  the precision in the Normal prior of the subnational-level intercept
  in single-country subnational models.
