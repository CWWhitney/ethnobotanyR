
<img src="vignettes/ethnobotanyR.png" alt="ethnobotanyR logo" align="right" width = "25%" height="25%"/>

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![CRAN
Status](https://www.r-pkg.org/badges/version/ethnobotanyR?color=yellow)](https://cran.r-project.org/package=ethnobotanyR)
[![](https://cranlogs.r-pkg.org/badges/grand-total/ethnobotanyR?color=orange)](https://cran.r-project.org/package=ethnobotanyR)
[![](https://cranlogs.r-pkg.org/badges/ethnobotanyR?color=blue)](https://cran.r-project.org/package=ethnobotanyR)
[![](https://cranlogs.r-pkg.org/badges/last-week/ethnobotanyR?color=green)](https://cran.r-project.org/package=ethnobotanyR)
<!-- badges: end -->

[!["Buy Me A
Coffee"](https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png)](https://www.buymeacoffee.com/cwhitney)

# ethnobotanyR

**Quantifying Traditional Ecological Knowledge for Decision-Making and Conservation Modeling**

Please remember to cite the `ethnobotanyR` package if you use it in your
publications. Use `citation("ethnobotanyR")` to get a citation for the
latest version.

    To cite package 'ethnobotanyR' in publications use:

      Whitney C (2022). _ethnobotanyR: Ethnobotanical Analysis, Decision-Framing, 
      and TEK Modeling_. doi:10.32614/CRAN.package.ethnobotanyR
      <https://doi.org/10.32614/CRAN.package.ethnobotanyR>, R package
      version 0.2.0, <https://CRAN.R-project.org/package=ethnobotanyR>.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {ethnobotanyR: Ethnobotanical Analysis, Decision-Framing, and TEK Modeling},
        author = {Cory Whitney},
        year = {2022},
        note = {R package version 0.2.0},
        url = {https://CRAN.R-project.org/package=ethnobotanyR},
        doi = {10.32614/CRAN.package.ethnobotanyR},
      }

## What This Package Does

`ethnobotanyR` provides tools to:

- **Quantify Traditional Ecological Knowledge (TEK)** using standard indices (Use Value, Relative Frequency of Citation, Relative Importance, etc.)
- **Model TEK in decision frameworks** using Bayesian networks and Monte Carlo sampling
- **Design and facilitate decision-framing exercises** in conservation and development contexts
- **Visualize and communicate** TEK distributions and decision-relevant uncertainty to stakeholders
- **Disaggregate knowledge** by stakeholder type, gender, geography, and other meaningful dimensions

## What This Package Does NOT Do

- **Make decisions for you.** Ethnobotanical indices and models describe what communities know and value; they don't determine conservation strategy or predict outcomes.
- **Offer robust proof through indices alone.** Standard quantitative ethnobotany indices have well-known scientific limitations. See `vignette("honest_ethnobotany")` for a critical assessment.
- **Replace participatory engagement.** Quantification is a tool for making knowledge visible and facilitating dialogue—not a substitute for honest, power-aware conversation about values and tradeoffs.

## Choose Your Path

### **I want to describe what my community knows and uses**

Start with: `vignette("ethnobotanyr_vignette")` 

Use cases:
- Calculate Use Value (UV), Relative Frequency of Citation (RFC), and other indices
- Compare knowledge across demographic groups
- Identify patterns in ethnobotanical knowledge
- **Important:** Read `vignette("honest_ethnobotany")` first to understand limitations of indices

**Key principle:** Indices are descriptive tools. Report them with transparency, disaggregate by meaningful groups, and avoid causal claims.

### **I want to model TEK in decision contexts**

Start with: `vignette("TEK_modeling_vignette")` 

Use cases:
- Elicit TEK as probability distributions (Beta, Dirichlet priors)
- Build Bayesian networks that propagate TEK-derived uncertainty
- Run Monte Carlo simulations to explore decision outcomes under uncertainty
- Integrate qualitative knowledge into quantitative decision models

**Key principle:** This is the robust approach. You're modeling uncertainty, not pretending precision where there is none.

### **I want to run a participatory decision-framing workshop**

Start with: `vignette("decision_framing_guide")` 

Use cases:
- Design multi-stakeholder engagement processes
- Clarify community priorities and surface disagreement
- Create decision spaces that honor multiple values
- Integrate ethnobotanical knowledge into community-led decisions
- Navigate power dynamics and structural constraints honestly

**Supporting resources:**
- `vignette("benin_case_study")` - Critical case study of fonio value-chain workshop
- `vignette("ethnobotanyr_decision_framing_practical")` - How to use ethnobotanyR functions in workshops
- `vignette("honest_ethnobotany")` - Critical assessment of what ethnobotanical data can and cannot show

<!-- Links: start -->

| Quick Links |
|:---|
| [**Installing ethnobotanyR**](https://github.com/CWWhitney/ethnobotanyR#Installation) |
| [**ethnobotanyR CRAN Version**](https://cran.r-project.org/package=ethnobotanyR) |
| [**Critical Assessment of Ethnobotany Indices**](vignettes/honest_ethnobotany.Rmd) |
| [**Decision-Framing Framework**](vignettes/decision_framing_guide.Rmd) |
| [**Benin Case Study**](vignettes/benin_case_study.Rmd) |
| [**TEK Modeling with Bayesian Networks**](vignettes/TEK_modeling_vignette.Rmd) |
| [**ethnobotanyR Wiki**](https://github.com/CWWhitney/ethnobotanyR/wiki) |

<!-- Links: end -->

## Installation

Install the released version of ethnobotanyR from
[CRAN](https://CRAN.R-project.org) with
`install.packages("ethnobotanyR")`.

Install the working version of ethnobotanyR from
[GitHub](https://github.com) with
`devtools::install_github("CWWhitney/ethnobotanyR")`.

## References

Albuquerque, Ulysses Paulino, Patricia Muniz de Medeiros, Washington
Soares Ferreira Junior, Taline Cristina da Silva, Rafael Ricardo
Vasconcelos da Silva, and Thiago Goncalves-Souza. 2019.
Social-Ecological Theory of Maximization: Basic Concepts and Two Initial
Models. Biological Theory.
[doi.org/10.1007/s13752-019-00316-8](https://doi.org/10.1007/s13752-019-00316-8).

Tardio, J., and M. Pardo-de-Santayana, 2008. Cultural Importance
Indices: A Comparative Analysis Based on the Useful Wild Plants of
Southern Cantabria (Northern Spain) 1. Economic Botany, 62(1), 24-39.
[doi.org/10.1007/s12231-007-9004-5](https://doi.org/10.1007/s12231-007-9004-5).

Whitney, C. W., Bahati, J., and Gebauer, J. (2018), Ethnobotany and
agrobiodiversity; valuation of plants in the homegardens of southwestern
Uganda. Ethnobiology Letters, 9(2), 90-100.
[doi.org/10.14237/ebl.9.2.2018.503](https://doi.org/10.14237/ebl.9.2.2018.503).

Whitney C (2022). *ethnobotanyR: Ethnobotanical Analysis, Decision-Framing, and TEK Modeling*. R package version 0.2.0,
<https://CRAN.R-project.org/package=ethnobotanyR>.
