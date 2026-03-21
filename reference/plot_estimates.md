# Wrapper function to plot the JAGS estimates

Wrapper function to plot the JAGS estimates

## Usage

``` r
plot_estimates(jagsdata, model_output)
```

## Arguments

- jagsdata:

  Output of the mcmsupply::get_modelinputs() function.

- model_output:

  The output of the mcmsupply::run_jags_model() function.

## Value

A list of ggplot objects.

## Examples

``` r
if (FALSE) { # \dontrun{
raw_data <- get_data(national=TRUE, local=TRUE, mycountry="Nepal")
jagsdata <- get_modelinputs(startyear=1990, endyear=2030.5, nsegments=12, raw_data)
mod <- run_jags_model(jagsdata, n_iter=5, n_burnin=1, n_thin=1)
plots <- plot_estimates(jagsdata = jagsdata, model_output = mod)
} # }
```
