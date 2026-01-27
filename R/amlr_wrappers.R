#' Wrapper function
#'
#' Wrapper functions with common AMLR defaults
#'
#' @param ... passed as-is to applicable base function
#' @param status passed to [shinydashboard::box()]. Default is `"warning"`
#' @param solidHeader passed to [shinydashboard::box()]. Default is `FALSE`
#' @param collapsible passed to [shinydashboard::box()]. Default is `TRUE`
#'
#' @details
#' `amlr_box` is a wrapper around [shinydashboard::box()],
#' with the following defaults: `status = "warning"`, `solidHeader = FALSE`,
#' `collapsible = TRUE`. The user can override these individually, or simply
#' use [shinydashboard::box()] itself.
#'
#' `tableNA` is a wrapper around the [base::table()] function,
#' with the `useNA` argument set to 'ifany'`.
#' Specifically, run the `table(..., useNA = 'ifany')`.
#'
#' @examples
#' tableNA(c(1, 2, NA, 2, NA, 2, 1, 3))
#' tableNA(c(1, 2, 2, 2, 1, 3))
#'
#' @name amlr_wrappers
#' @export
amlr_box <- function(...,
                     status = "warning",
                     solidHeader = FALSE,
                     collapsible = TRUE) {
  shinydashboard::box(
    ...,
    status = status, solidHeader = solidHeader, collapsible = collapsible
  )
}


#' @name amlr_wrappers
#' @export
tableNA <- function(...) table(..., useNA = 'ifany')
