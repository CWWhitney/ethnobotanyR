devtools::release()

## Test environments
* local OS Monterey 12.3.1, R.Version R 4.2.0 

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
I used the devtools::install_github("r-lib/revdepcheck") to run R CMD check with revdepcheck::revdep_check() on downstream dependencies. After running this I removed the revdep files to 
All packages passed 


## Resubmission
This is a resubmission. In this version I have:

- Added color options to the Radial_plot and ethno_alluvial functions with the rainbow package
- Added error checks for use observations with more than count '1'
- Added corrections for those same use observations
- Removed pbapply options
- Address issues with the gap.degree in chord plots (add a warning that more than 50 species or informants is a lot)
- Added a new vignette (split the existing into one about  indices and one about modeling and expanded on both)