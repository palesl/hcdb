
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
at [aushighcourtdatabase.org](https://aushighcourtdatabase.org).

## Citation

If you use this package, please cite:

Leslie, P., Robinson, Z., Smyth, R., & Jacobi, T. (2026). Supporting
comparative studies of judicial behavior: Introducing the Australian
High Court Database. *Journal of Law & Empirical Analysis*, *3*(1),
251–263. <https://doi.org/10.1177/2755323X261438254>

``` bibtex
@article{leslie2026hcdb,
  author  = {Pat Leslie and Zo\"{e} Robinson and Russell Smyth and Tonja Jacobi},
  title   = {Supporting Comparative Studies of Judicial Behavior: Introducing the Australian High Court Database},
  journal = {Journal of Law \& Empirical Analysis},
  year    = {2026},
  volume  = {3},
  number  = {1},
  pages   = {251--263},
  doi     = {10.1177/2755323X261438254}
}
```
