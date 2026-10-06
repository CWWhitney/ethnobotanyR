devtools::release()

## Test environments
* local macOS, R 4.5.1

## R CMD check results
`devtools::check(args = "--as-cran")`: 0 errors, 0 warnings, 0 notes.

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
