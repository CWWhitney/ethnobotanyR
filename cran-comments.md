devtools::release()

## Test environments
* local OS X install, R 3.5.2
* ubuntu 12.04 (on travis-ci), R 3.5.2
* win-builder (devel and release) `devtools::build_win()`

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs
I checked spelling with `devtools::spell_check()`
All is fine
I checked R-hub with `devtools::check_rhub()`
All is well
checked win-builder with `check_win_devel()`
All is ok

## Downstream dependencies
I have also run R CMD check `devtools::revdep_check()` on downstream dependencies 
All packages passed 

## Resubmission
This is a resubmission. In this version I have:

* Converted the DESCRIPTION title to title case.

* More clearly identified the copyright holders in the DESCRIPTION
  and LICENSE files.
