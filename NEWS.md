# ethnobotanyR News

# version 0.3.0 News

New models, data and tests; fixes to `ethno_bayes_consensus()` and `ethno_boot()`.

## Breaking change

- `ethno_bayes_consensus()`: responses must be whole numbers from 0 to `answers - 1` (0/1 for binary data) and result rows are labelled 0 and 1. Earlier results were wrong because 0 responses were ignored. Competence (`prior_for_answers`) is required.

## New functions and data

- `ethno_beta()`: Beta-binomial probability of use per species and use. Default for small samples and rare uses.
- `ethno_saturation()`: informant saturation curve.
- `ethno_consensus()`: consensus with competence estimated from the data (EM).
- `homegardens`: 102 homegardens in southwestern Uganda, 225 species, 14 uses, 3,961 use reports, as analyzed in Whitney et al. (2018). One informant per garden. Companion tables `homegardens_info` (garden covariates) and `homegardens_species` (life form, family, native).
- Tests (`testthat`).

## Bug fixes

- `ethno_bayes_consensus()`: corrected the likelihood for more than two answers.
- `ethno_boot()`: `n2` defaults to the number of observations. New `use_weights` argument. Warns when all observations are identical (zero-width interval).
- Vignettes no longer rewrite `.bib` files at build time.
- `honest_ethnobotany`: fixed swapped lower/upper labels.
- TEK modeling vignette: network chunk runs and compares decisions. Weighted pooling uses effective sample size. Removed arbitrary pseudo-count mapping.

## Vignettes

- Modeling and TEK vignettes use `homegardens`. The TEK vignette adds a network learned from the survey.

# version 0.2.0 News

Repositions the package from an indices calculator to a framework for integrating Traditional Ecological Knowledge (TEK) into conservation and development decisions.

## New vignettes

- `honest_ethnobotany`: what indices can and cannot show, and responsible use.
- `decision_framing_guide`: structured decision framing for community-based conservation.
- `benin_case_study`: critique of a fonio value-chain workshop in Benin.
- `ethnobotanyr_decision_framing_practical`: worked code for participatory workshops.
- `TEK_modeling_vignette`: Beta/Dirichlet pooling, Bayesian networks, Monte Carlo.

## README

- Three pathways: describe knowledge, model decisions, run participatory exercises.
- States what the package does not do: make decisions, prove with indices, replace engagement.

## Where to start

1. Participatory work: decision framing guide, Benin case study.
2. Before using indices: `honest_ethnobotany`.
3. Use indices to disaggregate and communicate, not predict.
4. High-stakes decisions: model uncertainty (TEK modeling vignette).

---

# version 0.1.9.2 News

Working version (hence the trailing '.2').

## Enhancements
- `TPL` handles infraspecific ranks more robustly.
- Faster processing of large species lists.
- Removed taxonomy steps and vignettes for now.

## Bug Fixes
- Fixed error for certain Genus-Species combinations.
- Fixed data format inconsistencies.

## References
- Whitney, C. W., Bahati, J., & Gebauer, J. (2018). Ethnobotany and Agrobiodiversity: Valuation of Plants in the Homegardens of Southwestern Uganda. Ethnobiology Letters, 9(2), 90–100. <https://doi.org/10.14237/ebl.9.2.2018.503> (checked against Crossref, 2026-10-06)

# version 0.1.9.1 News

Working version (hence the trailing '.1').

- New vignettes for species names and modeling.
- Scaling back the quantitative indices in favor of modeling.

# version 0.1.9 News

Patch (Whitney 2022). Updated for R 4.2.0.

## Enhancements
- Color options for `ethno_alluvial()` and `radial_Plot()`.
- Error checks and corrections for use counts above 1.
- New vignette `Modeling with ethnobotanyR`; existing vignette split into indices and modeling.
- More `ggplot2` output options.
- `ethno_boot()`: non-parametric bootstrap as a Bayesian model.

## Bug fixes
- Removed `pbapply` options.
- Fixed `gap.degree` in chord plots; warns above 50 species or informants.
- Removed `dplyr` arguments for the upcoming version.
- Addressed CRAN issue https://cran.r-project.org/web/checks/check_results_isoband.html

# version 0.1.8 News

Patch (Whitney 2021). Fixed bugs from Myanmar and China work; removed old functions for new tidyverse methods.

# version 0.1.7 News

Patch (Whitney 2020c). Added functions from Myanmar and China work; removed old functions.

# version 0.1.6 News

Patch (Whitney 2020b). Fixed bugs; more model and figure options.

# version 0.1.5 News

Patch (Whitney 2020a). Fixed bugs; more model and figure options.

# version 0.1.4 News

Patch. Fixed bugs; new methods, models and figures.

# version 0.1.3 News

Patch (Whitney 2019b). Fixed bugs; new indices and figures.

# version 0.1.2 News

Patch. Fixed bugs; new methods.

# version 0.1.1 News

Patch (Whitney 2019a). Fixed bugs; new indices.

# version 0.1.0 News

New release: 'Calculate Quantitative Ethnobotany Indices', common standard ethnobotany indices.

## References

Whitney, C. 2022, ethnobotanyR v0.1.9, Figshare. 10.6084/m9.figshare.21780890
Whitney, C. 2021, ethnobotanyR v0.1.8, Figshare. 10.6084/m9.figshare.13554029.v2
Whitney, C. 2020c, ethnobotanyR v0.1.7, Figshare. 10.6084/m9.figshare.11791830.v3
Whitney, C. 2020b, ethnobotanyR v0.1.6, Figshare. 10.6084/m9.figshare.9948620.v2
Whitney, C. 2020a, ethnobotanyR v0.1.5, Figshare. 10.6084/m9.figshare.9956345.v3
Whitney, C. 2019b, ethnobotanyR v0.1.3, Figshare. 10.6084/m9.figshare.9956336.v1
Whitney, C. 2019a, ethnobotanyR v0.1.1, Figshare. 10.6084/m9.figshare.8050529.v3
