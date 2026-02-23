devtools::release()

## Test environments
* local OS Sequoia 15.6.1, R.Version R 4.5.1 
Apple clang version 16.0.0 (clang-1600.0.26.6)
       GNU Fortran (GCC) 14.2.0

## R CMD check results
I ran devtools::build() and checked the package with devtools::check(), which also runs devtools::document()
There were no ERRORs, WARNINGs or NOTEs
All is well

I checked spelling with devtools::spell_check()
All is fine

I checked R-hub with devtools::check_rhub()
All is great

## Downstream dependencies
I accessed all dependencies with devtools::install_deps(dependencies = TRUE)
I used the devtools::install_github("r-lib/revdepcheck") to run R CMD check with revdepcheck::revdep_check() on downstream dependencies. All packages passed 
**After running this I removed the revdep files and submitted with devtools::submit_cran() **

## Resubmission
This is a patch. In this version I have:

- Improved the TPL function to handle infraspecific ranks more robustly.
- Enhanced speed when processing large species lists.
- Fixed an issue where the function returned an error with certain Genus-Species combinations.
- Resolved a bug related to data format inconsistencies.
- Ultimately removed all taxonomy step sand vignettes for now