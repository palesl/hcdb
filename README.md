
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hcdb

<!-- badges: start -->

<!-- badges: end -->

The goal of hcdb is to provide an easy way to load in data from the
Australian HCDB data project.

## Installation

You can install the development version of hcdb from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("palesl/hcdb")
```

## Example

How to use the data.

``` r
library(hcdb)

data<-justice_decision
```

If you’d like coded categorical data as factors (for analysis), then:

``` r
 
data<-haven::as_factor(justice_decision, only_labelled=TRUE)
```

If you prefer the data in Stata, SPSS, or Excel formats, you can access
it at [aushighcourtdatabase.org](https://aushighcourtdatabase.org).
