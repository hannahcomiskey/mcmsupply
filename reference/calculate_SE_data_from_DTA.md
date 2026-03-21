# Calculate Standard Errors and Variance-Covariance Matrices for DHS Subnational Estimates

This function reads a DHS dataset, constructs a survey design object,
computes weighted estimates of modern contraceptive method sources by
sector category, and exports both the proportion matrix and the
variance-covariance matrix to Excel files.

## Usage

``` r
calculate_SE_data_from_DTA(filepath, myresultsfolder)
```

## Arguments

- filepath:

  Character string. The path to the DHS `.dta` file.

- myresultsfolder:

  Character string. Path to a folder where Excel outputs will be saved.
  The function will create the required subfolders (`proportions/` and
  `varcov/`) if they do not exist.

## Value

A list with two elements (returned invisibly):

- prop_mat:

  Data frame of weighted estimates with counts

- vcov_matrix:

  Variance–covariance matrix tibble

Output files are written to disk as a side effect.

## Details

The function:

1.  Reads DHS survey microdata.

2.  Constructs derived variables such as modern method, sector, and
    method source.

3.  Builds a survey design using survey.

4.  Computes weighted proportions using `svyby()`.

5.  Extracts the associated variance–covariance matrix.

6.  Exports both datasets as Excel workbooks.

## Examples

``` r
if (FALSE) { # \dontrun{
calculate_SE_data("data/country.dta", "results/")
} # }
```
