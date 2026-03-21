# Running a subnational level single-country model with custom data

## Load your library

``` r
library(mcmsupply)
library(dplyr)
set.seed(1209)
```

## Try to use unsuitable data. The `get_data` function throws an error indicating that the Method column is missing from the custom user-supplied data.

``` r
cleaned_data <- get_data(national=FALSE, local=TRUE, 
                         surveydata_filepath = "~/Documents/R/mcmsupply/inst/data-raw/my_custom_data_bad.xlsx",
                         mycountry="Ethiopia")
```

    ## Error in `get_data()`:
    ## ! could not find function "get_data"

## Load the suitable data

``` r
cleaned_data <- get_data(national=FALSE, local=TRUE, 
                         surveydata_filepath = "~/Documents/R/mcmsupply/inst/data-raw/my_custom_data_good.xlsx",
                         mycountry="Ethiopia")
```

## Get the JAGS model inputs and the cleaned data

``` r
pkg_data <- get_modelinputs(startyear=1990, endyear=2025.5,
                            nsegments=12, raw_data = cleaned_data)
```

## Run JAGS model and get posterior point estimates with uncertainty

``` r
mod <- run_jags_model(jagsdata = pkg_data, jagsparams = NULL,
                      n_iter = 40000, n_burnin = 10000, n_thin = 15)
```

## Plot posterior point estimates with uncertainty

``` r
plots <- plot_estimates(jagsdata = pkg_data, model_output = mod)
```

## Pull out estimates that you are particularly interested in

``` r
estimates_2018 <- pull_estimates(model_output = mod, country = 'Ethiopia', year=2018)

head(estimates_2018)
```
