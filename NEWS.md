# ethnobotanyR News

# version 0.2.0 News

This is a major update repositioning the package from a quantitative indices calculator to a comprehensive framework for integrating Traditional Ecological Knowledge (TEK) into conservation and development decision-making.

## Major Changes

**New vignettes:**
- `vignette("honest_ethnobotany")` - Critical assessment of quantitative ethnobotany indices: what they can tell you, what they cannot tell you, when they're useful, and when they mislead. A no-holds-barred examination of both limitations and responsible use.
- `vignette("decision_framing_guide")` - Comprehensive framework for structured decision-framing in community-based conservation. Covers decision theory, participatory ethics, epistemological foundations, multiple contrasting cases (successes and failures), and principles for defensible practice.
- `vignette("benin_case_study")` - Critical case study of a fonio value-chain workshop in Benin. Shows what the workshop achieved, what it doesn't tell us, what follow-up is needed, and how to avoid treating a single successful event as a replicable methodology.
- `vignette("ethnobotanyr_decision_framing_practical")` - Practical guide with worked code examples for using ethnobotanyR functions in participatory decision-framing workshops. Shows how to calculate indices responsibly and use them as prompts for dialogue rather than decision-drivers.

**Reframed README:**
- Now emphasizes three user pathways (describe knowledge, model decisions, run participatory exercises) rather than indices as the primary product
- Explicitly states what the package does NOT do (make decisions, offer robust proof via indices, replace engagement)
- Directs users to `vignette("honest_ethnobotany")` before using indices
- Repositions indices as descriptive tools within larger frameworks

**Emphasis shift:**
- Moved from "indices as the main product" to "decision-framing and Bayesian modeling as primary methods"
- Indices now positioned as one tool for communication and exploration, not as foundational to conservation/development decisions
- Elevated Bayesian network modeling and structured decision-framing as the robust, defensible approaches

## What This Means

Users should now:
1. **Start with the framework vignettes if planning participatory work** (decision-framing guide, Benin case study)
2. **Read the honest critique before using indices** (honest_ethnobotany vignette)
3. **Use indices for disaggregation and communication, not prediction** (practical guide with examples)
4. **Model uncertainty when making high-stakes decisions** (TEK modeling with Bayesian networks)

The package now acknowledges both the utility and the substantial limitations of quantitative ethnobotany indices, and provides a path toward scientifically defensible, ethically grounded decision-making that integrates TEK.

---

# version 0.1.9.2 News

# This version 0.1.9.2 of ethnobotanyR is a working version (thus the redundant trailing '.2'):

## Enhancements
- Improved the TPL function to handle infraspecific ranks more robustly.
- Enhanced speed when processing large species lists.
- Ultimately removed all taxonomy step sand vignettes for now

## Bug Fixes
- Fixed an issue where the function returned an error with certain Genus-Species combinations.
- Resolved a bug related to data format inconsistencies.

## References
- Whitney, C. et al. (2018). Ethnobotany and Agrobiodiversity. Ethnobiology Letters, 9(2), 90–100.
      
# version 0.1.9.1 News

This version 0.1.9.1 of ethnobotanyR is a working version (thus the redundant trailing '.1'):

Adding new vignettes for species names and for modeling
Starting the process of dismantling the quantitative indices stuff in favor of some modeling - adding more modeling and removing - or at least scaling back argumentation about - the quantitative indices

# version 0.1.9 News

This version 0.1.9 of ethnobotanyR is a patch (Whitney 2022):

## Enhancements

Updated for R 4.2.0 "Vigorous Calisthenics":

- Added color options for `ethno_alluvial()` and `radial_Plot()` 
- Add error checks for use observations with more than count '1'
- Add corrections for those same use observations
- Added a new vignette `Modeling with ethnobotanyR` (split the existing into one about indices and one about modeling and expanded on both)
- Add more visual output options in `ggplot2` (...)
- Update the non-parametric bootstrap as a Bayesian Model `ethno_boot()`


## Bug fixes

- Removed pbapply options
- Address issues with the gap.degree in chord plots (add a warning that more than 50 species or informants is a lot)
- Remove arguments for dplyr (to work with version on the way)
- Address CRAN issue https://cran.r-project.org/web/checks/check_results_isoband.html

# version 0.1.8 News

This version 0.1.8 of ethnobotanyR (Whitney 2021) is a patch:

Fixed bugs in functions from Myanmar work and China work/ removed more old functions updated for new methods in the tidyverse

# version 0.1.7 News

This version 0.1.7 of ethnobotanyR (Whitney 2020c) is a patch:
Added new functions from Myanmar work and China work/ removed old functions

# version 0.1.6 News

This version 0.1.6 of ethnobotanyR (Whitney 2020b) is a patch:
Fixed some bugs / added some more model options and figure options

# version 0.1.5 News

This version 0.1.5 of ethnobotanyR (Whitney 2020a) is a patch:
Removed some bugs / added some more model options and figure options

# version 0.1.4 News

This version 0.1.4 of ethnobotanyR is a patch:
Removed some bugs and added some new methods/models and figures

# version 0.1.3 News

This version 0.1.3 of ethnobotanyR (Whitney 2019b)  is a patch:
Fixed some bugs and added some new indices and figures

# version 0.1.2 News

This version 0.1.2 of ethnobotanyR is a patch:
Removed some bugs / added some new methods

# version 0.1.1 News

This version 0.1.1 of ethnobotanyR (Whitney 2019a) is a patch:
Fixed some bugs and added some new indices

# version 0.1.0 News

This version 0.1.0 of ethnobotanyR is a new release
The package ethnobotanyR 'Calculate Quantitative Ethnobotany Indices' following some common standard indices used in ethnobotany. 

## References

Whitney, C. 2022, ethnobotanyR v0.1.9, Figshare. 10.6084/m9.figshare.21780890
Whitney, C. 2021, ethnobotanyR v0.1.8, Figshare. 10.6084/m9.figshare.13554029.v2
Whitney, C. 2020c, ethnobotanyR v0.1.7, Figshare. 10.6084/m9.figshare.11791830.v3
Whitney, C. 2020b, ethnobotanyR v0.1.6, Figshare. 10.6084/m9.figshare.9948620.v2
Whitney, C. 2020a, ethnobotanyR v0.1.5, Figshare. 10.6084/m9.figshare.9956345.v3
Whitney, C. 2019b, ethnobotanyR v0.1.3, Figshare. 10.6084/m9.figshare.9956336.v1
Whitney, C. 2019a, ethnobotanyR v0.1.1, Figshare. 10.6084/m9.figshare.8050529.v3
