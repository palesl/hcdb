#' Advocate bio data for HCA cases 1995-2019
#'
#' Biographical data from all representatives appearing before the High Court of Australia in full hearings, 1995-2019
#'
#' @format ## `advocate_case`
#' A tibble with 11,423 rows and 15 columns:
#' \describe{
#'   \item{justice}{HCDB justice ID number}
#'   \item{caseNumber}{HCA docket number}
#'   \item{caseName}{name of the case}
#'   ...
#' }
#' @source Robinson, Z., Leslie, P. (High Court Database)
#' @references Leslie, P., Robinson, Z., Smyth, R., & Jacobi, T. (2026). Supporting comparative
#'   studies of judicial behavior: Introducing the Australian High Court Database. *Journal of
#'   Law & Empirical Analysis*, *3*(1), 251–263. \doi{10.1177/2755323X261438254}
#' @seealso Full variable documentation: `vignette("advocate_case_codebook", package = "hcdb")`
"advocate_case"
