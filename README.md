# GSSU: An R package for linear models in the presence of treatment effect heterogeneity

This package implements the estimators and tests found in Gibbons, Su&#225;rez-Serrato and Urbancic, 2018, "Broken or Fixed Effects?" *Journal of Econometric Methods*. [[article](https://www.degruyter.com/view/j/jem.ahead-of-print/jem-2017-0002/jem-2017-0002.xml)].

## Installation
To install the package, first install `devtools` as you normally would, then load the package. Install `GSSU` using `devtools::install_github('gibbonscharlie/GSSU')`. Once the package is out of its early beta stage (see below), it will be uploaded to CRAN.

## Examples and documentation
See the help files for documentation, particularly those for `EstimateIWE`, `ScoreTest`, and `SpecTest`.

## Notes
Please note that this package is **still in development** and **very much in beta** right now. Calculations involving variances (standard errors, test statistics) do not yet line up with the Stata results due to small sample correction issues (in process of being harmonized). If you have any feedback or suggestions, feel free to reach out by submitting an issue above or by sending me an e-mail.