#' Determine AMLR season from date
#'
#' Determine AMLR season from date
#'
#' @param x object of class Date
#'
#' @details
#' Determine the AMLR season from the date.
#' Note that this just generates the season name from the date year,
#' with July as the demarcation,
#' rather than using the season_info table.
#'
#' @return
#' Character vector of length \code{x} of the calculated season names,
#' in the form 'YYYY/YY' (e.g., 2016/17)
#'
#' @examples
#' amlr_season(as.Date("1999-12-31"))
#' amlr_season(as.Date(c("2002-03-01", "2002-10-01")))
#' amlr_season(as.Date("2017-01-01"))
#'
#' @export
amlr_season <- function(x) {
  stopifnot(inherits(x, c("Date", "POSIXct")))

  # Use format to avoid more dependencies. Maybe worth it.
  mo <- as.numeric(format(x, "%m"))
  yr <- as.numeric(format(x, "%Y"))
  if_else(mo >= 7,
          paste(yr, substr(yr+1, 3, 4), sep = "/"),
          paste(yr-1, substr(yr, 3, 4), sep = "/"))
}
