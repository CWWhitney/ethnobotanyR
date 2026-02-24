devtools::release()

## Test environments
* local OS Sequoia 15.6.1, R.Version R 4.5.1 
Apple clang version 16.0.0 (clang-1600.0.26.6)
       GNU Fortran (GCC) 14.2.0

## R CMD check results
I built functions with roxygen2::roxygenise(),
I ran devtools::build(),
I checked the package with devtools::check(),
I ran devtools::document(),
There were no ERRORs, WARNINGs or NOTEs
All is well

I checked spelling with devtools::spell_check()
All is fine

## Downstream dependencies
I accessed all dependencies with devtools::install_deps(dependencies = TRUE)
I used the devtools::install_github("r-lib/revdepcheck") to run R CMD check with revdepcheck::revdep_check() on downstream dependencies. All packages passed 
**After running this I removed the revdep files and submitted with devtools::submit_cran() **


## Resubmission
This is a major update (v0.2.0) repositioning ethnobotanyR from a quantitative indices calculator to a comprehensive framework for integrating Traditional Ecological Knowledge (TEK) into conservation and development decision-making.

In this version I have:

- Added new vignettes: honest_ethnobotany (critical assessment of indices), decision_framing_guide (structured decision-framing), benin_case_study (case study), and ethnobotanyr_decision_framing_practical (practical guide for participatory workshops).
- Reframed the README to emphasize user pathways, clarify limitations, and direct users to critical vignettes before using indices.
- Shifted emphasis from indices as the main product to decision-framing and Bayesian modeling as primary methods; indices are now positioned as descriptive tools for communication, not as decision drivers.
- Elevated Bayesian network modeling and structured decision-framing as robust, defensible approaches for integrating TEK.
- Removed all code and vignette dependencies on AnthroTools (not on CRAN).
- Updated all URLs to their current, permanent locations (https, no redirects).
- Removed non-standard top-level files and directories (e.g., revdep).
- Updated rhub workflow to use rhub v2 syntax (local_check, cloud_check).