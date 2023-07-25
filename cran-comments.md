## Resubmission 1
- Added references
- Addressed issues with line breaks in DESCRIPTION file
- Added missing Rd-tag  to plot_estimates.Rd: `\value`
- Update: Changed `\dontrun{}` to `\dontttest{}` for examples
- Added shorter examples 
- Addressed issue with print()

## Resubmission 2
- Update: Changed `\dontrun{}` to `\dontttest{}` for examples, expect for the plot_estimates.R. In this example, if I change the command to `donttest{}` the examples fail as they cannot locate the JAGS model files. However, when I leave it as `dontrun{}` and run_jags_model.R as `donttest{}`, the examples are fine. I cannot get to the bottom of why this is the case. Any advice would be greatly appreciated. The code is passing all tests outside of the examples environment so I am confident that the code works correctly.

## Resubmission 3
- Updates to JAGS models following reviewers comments
- Updates to standard error imputation for national-level data
- Inclusion of a DHS design effect database for standard error imputation

## Test environments

local OS X install, R 4.3.1 GUI 1.79 Big Sur Intel build

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

* This is a new release.

Window tests and Mac release tests were successful.

The R-hub runs failed because it couldn't find JAGS but I think this is a missing dependency on R-hub rather than a problem with the package (or a missing option that I need to add in somehow). This is impacting the building of my vignettes resulting in errors that I cannot fix.

R-CMD-Checks on Github were successful.

