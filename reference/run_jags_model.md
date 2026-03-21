# Wrapper function to run the jags model for estimating the proportion of modern contraceptive methods supplied by the public & private Sectors using a Bayesian hierarchical penalized spline model for the national and subnational administration levels

Wrapper function to run the jags model for estimating the proportion of
modern contraceptive methods supplied by the public & private Sectors
using a Bayesian hierarchical penalized spline model for the national
and subnational administration levels

## Usage

``` r
run_jags_model(
  jagsdata,
  jagsparams = NULL,
  n_iter = 80000,
  n_burnin = 10000,
  n_thin = 35,
  n_chain = 2,
  n_cores = NULL,
  ...
)
```

## Arguments

- jagsdata:

  The object from the mcmsupply::get_modelinputs() function.

- jagsparams:

  The parameters of the JAGS model you wish to review

- n_iter:

  Default is 80000. Number of itterations to do in JAGS model.

- n_burnin:

  Default is 10000. Number of samples to burn-in in JAGS model.

- n_thin:

  Default is 35. Number of samples to thin by in JAGS model.

- n_chain:

  Default is 2. Number of chains to run in your MCMC sample.

- n_cores:

  The number of cores to use for parallel execution in subnational
  estimation. If not specified, the number of cores is set to the value
  of options("cores"), if specified, or to approximately half the number
  of cores detected by the parallel package.

- ...:

  Arguments from the mcmsupply::get_modelinputs() function.

## Value

returns the jags model object

## Examples

``` r
# \donttest{
raw_data <- get_data(national=TRUE, local=TRUE, mycountry="Nepal")
#> Using preloaded dataset!
#> Joining with `by = join_by(Country)`
#> Joining with `by = join_by(Country)`
#> Joining with `by = join_by(Country, average_year, Method, Super_region)`
#> Getting data for Nepal
jagsdata <- get_modelinputs(startyear=1990, endyear=2025.5, nsegments=12, raw_data)
run_jags_model(jagsdata, n_iter=5, n_burnin=1, n_thin=1)
#> Using preloaded dataset!
#> Joining with `by = join_by(Country)`
#> Joining with `by = join_by(Country)`
#> Joining with `by = join_by(Country, average_year, Method, Super_region)`
#> Error in dimnames(median_alpha_region_intercepts) <- `*vtmp*`: length of 'dimnames' [3] not equal to array extent
# }
```
