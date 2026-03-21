# Function to pull method-supply share median estimates and credible intervals for a given year and country.

Function to pull method-supply share median estimates and credible
intervals for a given year and country.

## Usage

``` r
pull_estimates(model_output, year, country)
```

## Arguments

- model_output:

  The output of the mcmsupply::run_jags_model() function.

- year:

  Numeric. The year of model estimated you wish to pull.

- country:

  String. The name of the country you wish to inspect.

## Value

A dataframe of model estimates for each method, with the median (50%),
80% and 95% credible intervals.

## Examples

``` r
if (FALSE) { # \dontrun{
raw_data <- get_data(national=TRUE, local=TRUE, mycountry="Nepal")
jagsdata <- get_modelinputs(startyear=1990, endyear=2025.5, nsegments=12, raw_data)
mod <- run_jags_model(jagsdata = jagsdata, jagsparams = NULL, n_iter = 5, n_burnin = 1, n_thin = 1)
estimates <- pull_estimates(model_output = mod, year=2018, country="Nepal")
} # }
```
