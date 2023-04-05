devtools::document()
devtools::build(vignettes = FALSE)
#devtools::build_vignettes()
# devtools::install(build_vignettes = TRUE)
devtools::install()
.rs.restartR()

devtools::load_all()
?mcmsupply::national_FPsource_data
mcmsupply::Afghanistan_neighbouradj
