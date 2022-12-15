# ethnobotanyR News

This version of ethnobotanyR is a patch:

# Enhancements

Updated for R 4.2.0 "Vigorous Calisthenics":

- Added color options for the alluvial and radial plots
- Add error checks for use observations with more than count '1'
- Add corrections for those same use observations
- Added a new vignette (split the existing into one about  indices and one about modeling and expanded on both)
- Add more visual output options ()
- Update the non-parametric bootstrap as a Bayesian Model


# Bug fixes

- Removed pbapply options
- Address issues with the gap.degree in chord plots (add a warning that more than 50 species or informants is a lot)
- Remove arguments for dplyr (to work with version on the way)
- Address CRAN issue https://cran.r-project.org/web/checks/check_results_isoband.html