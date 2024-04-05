## Update 2

  Status: OK
   
── R CMD check results ──────────────────────────────────────────────────────────────────────────────── mcmsupply 1.0.1 ────
Duration: 1m 35s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

- Updates to national data for bivariate logit data following Journal of Royal Statistical Society, Series A: Statistics in Society
- Updates to stored single-country national parameters for bivariate setting
- Updates to national model (multi-country and single-country) to include covariance between estimated proportions in the likelihood
- Improvement of coding logic following R Journal reviewer comments
- Inclusion of 'pull_estimates' function and 'get_posterior_P_samples' function to aid users when reviewing the estimates and output of models.

## Update 1

── R CMD check results ─────────────────────────────────────────────────────────────────────────────────────────────────────── mcmsupply 0.2.2 ────
Duration: 1m 35.3s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

Passing all checks on Github. 

- Updates to data for new single-country parameters
- Updates to data for multi-country correlations 
- Updates for Custom data bug fix
- Removed truncated data - no longer required for illustration

## Resubmission 3
- Updates to JAGS models following reviewers comments
- Updates to standard error imputation for national-level data
- Inclusion of a DHS design effect database for standard error imputation

## Resubmission 2
- Update: Changed `\dontrun{}` to `\dontttest{}` for examples, expect for the plot_estimates.R. In this example, if I change the command to `donttest{}` the examples fail as they cannot locate the JAGS model files. However, when I leave it as `dontrun{}` and run_jags_model.R as `donttest{}`, the examples are fine. I cannot get to the bottom of why this is the case. Any advice would be greatly appreciated. The code is passing all tests outside of the examples environment so I am confident that the code works correctly.

## Resubmission 1
- Added references
- Addressed issues with line breaks in DESCRIPTION file
- Added missing Rd-tag  to plot_estimates.Rd: `\value`
- Update: Changed `\dontrun{}` to `\dontttest{}` for examples
- Added shorter examples 
- Addressed issue with print()

## Test environments

local OS X install, R 4.3.1 GUI 1.79 Big Sur Intel build

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

* This is a new release.

Window tests and Mac release tests were successful.

The R-hub runs failed because it couldn't find JAGS but I think this is a missing dependency on R-hub rather than a problem with the package (or a missing option that I need to add in somehow). This is impacting the building of my vignettes resulting in errors that I cannot fix.

R-CMD-Checks on Github were successful.

