devtools::release()

## Test environments
* local OS Mojave 10.14.5, `R.Version()` R 3.6.1 

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs
I checked the package with `devtools::check()`
All is well
I checked spelling with `devtools::spell_check()`
All is fine
I checked R-hub with `devtools::check_rhub()`
All is great
checked win-builder with `devtools::check_win_devel()`
All is ok

## Downstream dependencies
I have also run R CMD check `revdepcheck::revdep_check()` on downstream dependencies 
All packages passed 

## Resubmission
This is a resubmission. In this version I have:

*Added new functions for quantitative assessment
*Updated UVs() following Tardio and Pardo-de-Santayana (2008)
*created simple_UVs() to calculate a simple UVs cf. Albuquerque et al. (2006).
*Added fidelity level per species FLs() from Friedman et al. (1986).
*Included the CVe() from Reyes-Garcia et al. (2006).
*Removed the ethnobotanyChord() function and dissagregated it into ethnoChord() and ethnoChordUser() functions to allow for plotting simple circos plots of the uses and users per species.
*Removed assertthat functions in favor of standard if() stop() solutions