#' justice decision data, 1995-2020
#'
#' Data from all justice decisions in all cases in the High Court of Australia, 1995-2020
#'
#' @format ## `justice_decision`
#' A tibble with 12,726 rows and 319 columns:
#' \describe{
#'   \item{HCDBcaseId}{HCDB case ID number}
#'   \item{clrCite}{Case Law Research citation}
#'   \item{aljrCite}{Australian Law Journal Reports citation}
#'   ...
#' }
#' @source Robinson, Z., Leslie, P. (High Court Database)
#' @references Leslie, P., Robinson, Z., Smyth, R., & Jacobi, T. (2026). Supporting comparative
#'   studies of judicial behavior: Introducing the Australian High Court Database. *Journal of
#'   Law & Empirical Analysis*, *3*(1), 251–263. \doi{10.1177/2755323X261438254}
#' @seealso Full variable documentation: `vignette("justice_decision_codebook", package = "hcdb")`,
#'   `vignette("justice_decision_core_codebook", package = "hcdb")`
"justice_decision"
