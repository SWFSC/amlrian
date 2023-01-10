#' AMLR seasons and dates
#'
#' Determine AMLR season from date, or vice versa
#'
#' @param x object of class Date
#' @param season.name character; season name. The format must be \code{YYYY/YY}
#' @param m month as numeric or character; abbreviation is ok
#' @param d numeric; day number in month
#'
#' @details
#' \code{amlr_season_from date} will determine the AMLR season from the date,
#' while \code{amlr_date_from_season} takes in a season name, month, and day
#' and returns a date object created using the month, day,
#' and the correct year extracted from the seaon name.
#' Note that these functions use July (month 7) as the demarcator,
#' rather than using the ***REMOVED*** season_info table.
#'
#' @return
#' Character vector of length \code{x} of the calculated season names,
#' in the form 'YYYY/YY' (e.g., 2016/17), or a date object
#'
#' @examples
#' amlr_season_from_date(as.Date("1999-12-31"))
#' amlr_season_from_date(as.Date("2017-01-01"))
#' amlr_season_from_date(as.Date(c("2002-03-01", "2002-10-01")))
#'
#' amlr_date_from_season("1999/00", 3, 4)
#' amlr_date_from_season("1999/00", 12, 4)
#' amlr_date_from_season(c("1996/97", "1999/00", "2016/17"), 10, 28)
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
    nchar(season.name) == 7
    # (m %in% 1:12) | (m %in% month.abb) | (m %in% month.name),
    # (day %in% days_in_month(m))
  )

  # Check validity of month value, and get numeric month
  m.num <- if (m %in% 1:12) {
    m
  } else if (m %in% month.abb) {
    which(m == month.abb)
  } else if (m %in% month.name) {
    which(m == month.name)
  } else {
    stop("Invalid value for m; it must be a numeric (1:12), ",
         "abbreviation (base::month.abb), ",
         "or full month name (base::month.name)")
  }

  # Check validity of day value
  if (!(d <= days_in_month(m.num)))
    stop("d must be less than or equal to the number of days in month m")

  # Return date created using the first or second part of the season name
  season.name.split <- strsplit(season.name, "/")
  season.name.curr <- if (m.num >= 7) {
    vapply(season.name.split, function(i) i[1], "1")
  } else {
    vapply(season.name.split, function(i) {
      if_else(i[1] == "1999", "2000", paste0("20", i[2]))
    }, "1")
  }

  ymd(paste(season.name.curr, m, d))
}
