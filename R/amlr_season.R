#' AMLR seasons and dates
#'
#' Determine AMLR season from date, or vice versa
#'
#' @param x object of class Date
#' @param season.name character; season name. The format must be \code{YYYY/YY}
#' @param m month as numeric or character; abbreviation is ok
#' @param d numeric; day number in month
#'
#' @details \code{amlr_season_from date} will determine the AMLR season from the
#'   date, while \code{amlr_date_from_season} takes in a season name, month, and
#'   day and returns a date object created using the month, day, and the correct
#'   year extracted from the seaon name. Note that these functions use July
#'   (month 7) as the demarcator, rather than using the ***REMOVED***
#'   season_info table.
#'
#'   For \code{amlr_date_from_season}, \code{season.name}, \code{m}, and
#'   \code{d} can be vectors, but must all be the same length
#'
#' @return Character vector of length \code{x} of the calculated season names,
#'   in the form 'YYYY/YY' (e.g., 2016/17), or a date object
#'
#' @examples
#' amlr_season_from_date(as.Date("1999-12-31"))
#' amlr_season_from_date(as.Date("2017-01-01"))
#' amlr_season_from_date(as.Date(c("2002-03-01", "2002-10-01")))
#'
#' amlr_date_from_season("1999/00", 3, 4)
#' amlr_date_from_season("1999/00", 12, 4)
#' amlr_date_from_season(c("1996/97", "2016/17"), c(3, 10), c(28, 19))
#'
#' @name amlr_season
#'
#' @export
amlr_season_from_date <- function(x) {
  stopifnot(inherits(x, c("Date", "POSIXct")))

  # Use format to avoid more dependencies. Maybe worth it.
  m <- as.numeric(format(x, "%m"))
  y <- as.numeric(format(x, "%Y"))
  if_else(m >= 7,
          paste(y, substr(y+1, 3, 4), sep = "/"),
          paste(y-1, substr(y, 3, 4), sep = "/"))
}

#' @name amlr_season
#' @export
amlr_date_from_season <- function(season.name, m, d) {
  stopifnot(
    nchar(season.name) == 7,
    length(season.name) == length(m),
    length(season.name) == length(d)
  )

  # Check validity of month value, and get numeric month
  m.num <- if (all(m %in% 1:12)) {
    m
  } else if (all(m %in% month.abb)) {
    vapply(m, function(i) which(i == month.abb), 1)
  } else if (all(m %in% month.name)) {
    vapply(m, function(i) which(i == month.name), 1)
  } else {
    stop("Invalid value for m; it must be a numeric (1:12), ",
         "abbreviation (base::month.abb), ",
         "or full month name (base::month.name)")
  }

  # Check validity of day value
  if (!all(as.numeric(d) <= days_in_month(m.num)))
    stop("d must be less than or equal to the number of days in month m")

  # Return date created using the first or second part of the season name
  season.name.split <- strsplit(season.name, "/")
  season.name1 <- vapply(season.name.split, function(i) i[1], "1")
  season.name2 <- vapply(season.name.split, function(i) {
    if_else(i[1] == "1999", "2000", paste0(substr(i[1], 1, 2), i[2]))
  }, "1")

  ymd(paste(if_else(m.num >= 7, season.name1, season.name2), m, d))
}
