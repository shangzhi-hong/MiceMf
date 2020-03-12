
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MiceMf

## Random-forest-based Single Imputation adapted for MICE

## Installation for development version from GitHub

Currently the `MiceMf` package is only available as source files on
GitHub, any interested user can install and test the `MiceMf` package by
running the following code in R:

``` r
if(!"remotes" %in% installed.packages()) install.packages("remotes")
remotes::install_github("shangzhi-hong/MiceMf")
```

## Example

``` r
library(MiceMf)
# Perform imputation
data("airquality")
impObj <- mice(airquality, method = "mf", m = 1, printFlag = FALSE)
impObjNorm <- mice(airquality, method = "mf.norm", m = 1, printFlag = FALSE)
# Get the complete data
resCmp <- complete(impObj, action = 1L)
resCmpNorm <- complete(impObjNorm, action = 1L)
```
