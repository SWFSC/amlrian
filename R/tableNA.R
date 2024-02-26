#' Wrapper around table function
#'
#' Wrapper around table function
#'
#' @param ... arguments passes to \code{\link[base]{table}}
#'
#' @details Run the \code{\link[base]{table}} function on \code{...}, while also
#' passing the argument \code{useNA = 'ifany'}
#'
#' @examples
#' tableNA(c(1, 2, NA, 2, NA, 2, 1, 3))
#' tableNA(c(1, 2, 2, 2, 1, 3))
#'
#' @export
tableNA <- function(...) table(..., useNA = 'ifany')
