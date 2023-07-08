## Resubmission 
- Added references
- Addressed issues with line breaks in DESCRIPTION file
- Added missing Rd-tag  to plot_estimates.Rd: `\value`
- Changed `\dontrun{}` to `\dontttest{}` for examples
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
