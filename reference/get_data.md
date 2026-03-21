# Wrapper function that retrieves the DHS data used for modelling the proportion of modern contraceptives supplied by the public and private sectors at the national and subnational administration levels.

Wrapper function that retrieves the DHS data used for modelling the
proportion of modern contraceptives supplied by the public and private
sectors at the national and subnational administration levels.

## Usage

``` r
get_data(
  national = TRUE,
  local = FALSE,
  mycountry = NULL,
  fp2030 = TRUE,
  surveydata_filepath = NULL
)
```

## Arguments

- national:

  TRUE/FALSE. Default is TRUE for national administration level data.
  FALSE retrieves subnational level data.

- local:

  TRUE/FALSE. Default is FALSE for global runs. Decides if this is a
  single-country or global run.

- mycountry:

  The name of country of interest. Default is NULL. For the names of
  potential countries, review vigentte.

- fp2030:

  Default is TRUE. Filter raw data to only include the Family Planning
  2030 focus countries discussed in the Comiskey et al. paper.

- surveydata_filepath:

  Path to survey data. Default is NULL. Survey data should be a .xlsx
  with the following format
  [`national_FPsource_data`](https://hannahcomiskey.github.io/mcmsupply/reference/national_FPsource_data.md).

## Value

returns a list containing the DHS data set used for inputs into the
model and the arguments that specify the data set up.

## Examples

``` r
# \donttest{
raw_data <- get_data(national=FALSE, local=TRUE, mycountry="Nepal")
#> Using preloaded dataset!
#> Joining with `by = join_by(Country, average_year)`
#> Getting data for Nepal
#> Error in dplyr::select(., `Country or area`, Region): Can't select columns that don't exist.
#> ✖ Column `Country or area` doesn't exist.
# }
```
