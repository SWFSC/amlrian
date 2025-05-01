#' Wrapper around table function
#'
#' Wrapper around table function
#'
#' @param ... arguments passed to [base::table()]
#'
#' @details
#' Wrapper around table function to automatically pass `useNA = 'ifany'`.
#'
#' Specifically, run the [base::table()] function on `...`, while also passing
#' the argument `useNA = 'ifany'`
#'
#' @examples
#' tableNA(c(1, 2, NA, 2, NA, 2, 1, 3))
#' tableNA(c(1, 2, 2, 2, 1, 3))
#'
#' @export
tableNA <- function(...) table(..., useNA = 'ifany')
