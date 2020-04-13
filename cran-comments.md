devtools::release()

## Test environments
* local OS Catalina 10.15.3, R.Version R 3.6.3 

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
I have also run R CMD check revdepcheck::revdep_check() on downstream dependencies 
All packages passed 

## Resubmission
This is a resubmission. In this version I have:

*Added new functions for quantitative assessment
