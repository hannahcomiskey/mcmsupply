# Function to pull the complete posterior sample for the national method-supply share estimates. Functionality for the subnational models is still under development.

Function to pull the complete posterior sample for the national
method-supply share estimates. Functionality for the subnational models
is still under development.

## Usage

``` r
get_posterior_P_samps(jagsdata, model_output, nposterior)
```

## Arguments

- jagsdata:

  The inputs for the JAGS model

- model_output:

  The output of the mcmsupply::run_jags_model() function.

- nposterior:

  The number of posterior samples you wish to pull.

## Value

A dataframe containing the posterior samples of national method-supply
share estimates.

## Examples

``` r
if (FALSE) { # \dontrun{
raw_data <- get_data(national=TRUE, local=TRUE, mycountry="Nepal")
jagsdata <- get_modelinputs(startyear=1990, endyear=2025.5, nsegments=12, raw_data)
mod <- run_jags_model(jagsdata = jagsdata, jagsparams = NULL, n_iter = 5, n_burnin = 1, n_thin = 1)
post_samps <- get_posterior_P_samps(jagsdata = jagsdata, model_output = mod, nposterior=4)
} # }
```
