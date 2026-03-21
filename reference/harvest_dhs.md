# Harvest DHS Dataset Availability

Scrapes the DHS Program website to retrieve information about the newest
available datasets.

## Usage

``` r
harvest_dhs(
  url = "https://dhsprogram.com/data/available-datasets.cfm",
  survey_column = "Survey",
  standard_dhs = TRUE
)
```

## Arguments

- url:

  Character string. URL of the DHS available datasets page. Default is
  "https://dhsprogram.com/data/available-datasets.cfm".

- survey_column:

  Character string. Name of the column containing survey information.
  Default is 'Survey'.

- standard_dhs:

  Logical. If TRUE, filters results to include only Standard DHS
  surveys. Default is TRUE.

## Value

A tibble containing all tables from the DHS program URL, with added
Country and Year columns extracted from the survey column.

## Examples

``` r
if (FALSE) { # \dontrun{
dhs_data <- harvest_dhs()
} # }
```
