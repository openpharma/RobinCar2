
<!-- markdownlint-disable-file -->
<!-- README.md needs to be generated from README.Rmd. Please edit that file -->

# RobinCar2

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN
status](https://www.r-pkg.org/badges/version-last-release/RobinCar2)](https://www.r-pkg.org/badges/version-last-release/RobinCar2)
[![CRAN monthly
downloads](https://cranlogs.r-pkg.org/badges/RobinCar2)](https://cranlogs.r-pkg.org/badges/RobinCar2)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/RobinCar2)](https://cranlogs.r-pkg.org/badges/grand-total/RobinCar2)
[![Code
Coverage](https://raw.githubusercontent.com/openpharma/RobinCar2/_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/openpharma/RobinCar2/_xml_coverage_reports/data/main/coverage.xml)
<!-- badges: end -->  

RobinCar2 is a package to provide robust inference of covariate adjusted
analysis under stratified randomization. The methods implemented are
recommended by [FDA guidance on covariate
adjustment](https://www.regulations.gov/docket/FDA-2019-D-0934). The
methods are based on the work of [Ye, Shao, Yi and Zhao
(2023)](doi:10.1080/01621459.2022.2049278), [Ye, Bannick, Yi and Shao
(2023)](doi:10.1080/24754269.2023.2205802), [Ye, Shao, Yi
(2023)](doi:10.1093/biomet/asad045%3E) and [Bannick, Shao, Liu, Du, Yi,
Ye (2024)](doi:10.48550/arXiv.2306.10213).

## Installation

### Development

You can install the current development version from `github` with:

``` r
if (!require("remotes")) {
  install.packages("remotes")
}

remotes::install_github(
  "openpharma/RobinCar2"
)
```

## Citing `RobinCar2`

To cite `RobinCar2` please see
[here](https://openpharma.github.io/RobinCar2/main/authors.html#citation).
