devtools::release()

## Test environments
* local OS Monterey 12.0.1, R.Version R 4.1.2 

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs
I ran devtools::document() 
and 
checked the package with devtools::check()
All is well

I checked spelling with devtools::spell_check()
All is fine

I checked R-hub with devtools::check_rhub()
All is great

checked win-builder with devtools::check_win_devel()
All is ok

## Downstream dependencies
I used the devtools::install_github("r-lib/revdepcheck") to run R CMD check with revdepcheck::revdep_check() on downstream dependencies 
All packages passed 


## Resubmission
This is a resubmission. In this version I have:

- Added color options to the Radial_plot and ethno_alluvial functions with the rainbow package
- Add error checks for use observations with more than count '1'
- Add corrections for those same use observations
- Removed pbapply options