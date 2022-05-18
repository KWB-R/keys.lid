[![R-CMD-check](https://github.com/KWB-R/keys.lid/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/keys.lid/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/keys.lid/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/keys.lid/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/keys.lid/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/keys.lid)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/keys.lid)]()

R Package for Simulating the Impact of Different
LIDs (Low Impact Development) under Varying Climate Boundary
Conditions in China on annual VRR (Volume Rainfall Retention).

## Installation

For installing the latest release of this R package run the following code below:

```r
# Enable repository from kwb-r
options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
  
# Download and install keys.lid in R
install.packages('keys.lid')

# Browse the keys.lid manual pages
help(package = 'keys.lid')
```

## Workflow 

The R workflow for assessing the hydraulic/hydrological performance of 
three low impact developments (`bioretention cells`, `green roofs`, `permeable pavements`) 
for different LID design parameters and climatic boundary conditions (five different 
climate zones in China) documented in the [Scenarios](articles/scenarios.html) 
article, which covers:

1. **Generation of more than 400 SWMM model setups** (defined in `keys.lid::read_scenarios()`), 

2. **Automatically run SWMM for all of them** from within R using `keys.lid::simulate_performances()` and finally 

3. **Temporally aggregate** (i.e. `annual rainfall retention`) **and visualise the results**.



