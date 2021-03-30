[![R-CMD-check](https://github.com/KWB-R/keys.lid/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/keys.lid/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/keys.lid/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/keys.lid/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/keys.lid/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/keys.lid)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/keys.lid)]()

R Package for Simulating the Impact of Different
LIDs (Low Impact Development) under Varying Climate Boundary
Conditions in China on annual VRR (Volume Rainfall Retention).

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'keys.lid' from GitHub
remotes::install_github("KWB-R/keys.lid")
```
