# Check Data Freshness Against DHS Reference

Compares local dataset against DHS reference data to determine if newer
surveys are available or exist but are not yet publicly available.

## Usage

``` r
check_data_freshness(
  local = NULL,
  reference = NULL,
  survey_column = "Survey Datasets",
  data_available_value = "Data Available"
)
```

## Arguments

- local:

  Data frame. Local dataset containing Country and Year columns. If
  NULL, uses `national_FPsource_data` from the mcmsupply package.
  Default is NULL.

- reference:

  Data frame. Reference dataset containing Country, Year, and survey
  status columns. If NULL, calls
  [`harvest_dhs()`](https://hannahcomiskey.github.io/mcmsupply/reference/harvest_dhs.md)
  to retrieve current DHS data. Default is NULL.

- survey_column:

  Character string. Name of the column in reference data indicating
  survey dataset availability. Default is 'Survey Datasets'.

- data_available_value:

  Character string. Value in survey_column that indicates data is
  publicly available. Default is 'Data Available'.

## Value

A data frame with columns Country, Year.package, Year.most_recent,
Year.most_recent_available, and Status. Status indicates whether the
local data is 'Up To Date', 'More Recent Available' (publicly
available), or 'More Recent Exists' (exists but not yet publicly
available).

## Examples

``` r
if (FALSE) { # \dontrun{
freshness <- check_data_freshness()
} # }
```
