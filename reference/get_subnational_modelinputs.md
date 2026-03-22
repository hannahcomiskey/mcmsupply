# Get JAGS model inputs

Get JAGS model inputs

## Usage

``` r
get_modelinputs(startyear = 1990, endyear = 2030.5, nsegments = 12, raw_data)
```

## Arguments

- startyear:

  The year you wish to begin your predictions from. Default is 1990.

- endyear:

  The year you wish to finish your predictions. Default is 2030.5.

- nsegments:

  The number of knots you wish to use in your basis functions. Default
  is 12.

- raw_data:

  The list of arguments and family planning source data from the
  'get_data' function.

## Value

A list of modelling inputs for the JAGS model.

1.  Tstar is the year index for the most recent survey in each province.

2.  Kstar is the knot index that aligns with Tstar.

3.  B.ik are the basis functions.

4.  n_years are total number of years

5.  n_obs are the total number of observations

6.  K are the number of knots.

7.  H is K-1. Used in the calculation of first order differences of
    spline coefficients.

8.  P_count is the number of subnational provinces/regions.

9.  M_count is the number of modern contraceptive methods.

10. matchsubnat is the subnational province indexing to match the
    observed data to the predictions.

11. matchcountry is the country indexing to match the observed data to
    the predictions.

12. matchmethod is the method indexing to match the observed data to the
    predictions.

13. matchyears is the year indexing to match the observed data to the
    predictions.

## Examples

``` r
# \donttest{
raw_data <- get_data(national=FALSE, local=TRUE, mycountry="Nepal")
#> Using preloaded dataset!
#> Joining with `by = join_by(Country, average_year)`
#> Getting data for Nepal
#> Joining with `by = join_by(Country)`
jagsdata <- get_modelinputs(startyear=1990, endyear=2030.5, nsegments=12, raw_data)
#> Adding missing grouping variables: `Country`, `index_country`
# }
```
