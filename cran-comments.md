<!-- Release workflow (devtools 2.5+):
usethis::use_release_issue()  # creates the checklist; work through it
devtools::document(); devtools::test(); devtools::check(args = "--as-cran")
urlchecker::url_check(); devtools::spell_check()
devtools::check_win_devel(); rhub::rhub_check()
devtools::submit_cran()  # then confirm by email; tag the release after acceptance
Deprecated, do not use: devtools::release(), build_vignettes(), check_rhub(), test_file(), reload(), create(), github_release()
-->

## Test environments
* local macOS, R 4.5.1

## R CMD check results
`devtools::check(args = "--as-cran")`: 0 errors, 0 warnings, 0 notes.

`rcmdcheck::rcmdcheck(args = "--as-cran")`: 0 errors, 0 warnings, 1 note. The note is local only: HTML validation skipped because the installed HTML Tidy is old.

`urlchecker::url_check()`: all URLs correct.

`devtools::spell_check()`: only names, citations and technical terms flagged.

## Downstream dependencies
Not rerun for this release.

## Update
This is v0.3.0, an update of v0.2.0 (on CRAN).

- New functions: `ethno_beta()`, `ethno_saturation()`, `ethno_consensus()`.
- New data: `homegardens`, `homegardens_info`, `homegardens_species` (published survey data, cited in the documentation).
- Fixed `ethno_bayes_consensus()` (0 responses were ignored; likelihood corrected). Responses must now be coded 0 to `answers - 1`. This changes results; earlier results were wrong.
- `ethno_boot()`: new `use_weights` argument; `n2` defaults to the number of observations.
- Added tests (`testthat`) and rewrote the modeling vignettes to use the new data.
