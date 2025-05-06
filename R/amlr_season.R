#' AMLR seasons and dates
#'
#' Determine AMLR season from a given date, or vice versa
#'
#' @param x object of class Date
#' @param season.name character; season name. The format must be `YYYY/YY`
#' @param m month as numeric or character; abbreviation is ok
#' @param d numeric; day number in month
#'
#' @details
#' `amlr_season_from_date()` will determine the AMLR season from the date, while
#' `amlr_date_from_season()` takes in a season name, month, and day and returns
#' a date object created using the month, day, and the year extracted from the
#' season name. Note that these functions use July (month 7) as the season
#' demarcation line, rather than using the database season_info table.
#'
#' `amlr_year_from_season()` determines the 'AMLR year' from the season name.
#' The AMLR year is the year that begins in January. For instance, for the
#' '2000/01' season, the AMLR year is '2001.
#'
#' For `amlr_date_from_season()`, `season.name`, `m`, and `d` can be vectors,
#' but a) must either a) all be the same length or b) `m` and `d` must be of
#' length one
#'
#' @return
#' `amlr_season_from_date()` returns a character vector of length `x` of the
#' calculated season names, in the form 'YYYY/YY' (e.g., 2016/17).
#' `amlr_date_from_season()` returns a date vector of the same length as
#' `season.name`. `amlr_year_from_season()` returns an integer vector of the
#' same length as `season.name`.
#'
#' @examples
#' amlr_season_from_date(as.Date("1999-12-31"))
#' amlr_season_from_date(as.Date("2017-01-01"))
#' amlr_season_from_date(as.Date(c("2002-03-01", "2002-10-01")))
#'
#' amlr_date_from_season("1999/00", 3, 4)
#' amlr_date_from_season("1999/00", 12, 4)
#' amlr_date_from_season(c("1996/97", "2016/17"), c(3, 10), c(28, 19))
#' amlr_date_from_season(c("1996/97", "2016/17"), 1, 1)
#'
#' amlr_year_from_season("2016/17")
#' amlr_year_from_season(c("1998/99", "1999/00", "2000/01"))
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
          paste(y, str_sub(y+1, 3, 4), sep = "/"),
          paste(y-1, str_sub(y, 3, 4), sep = "/"))
}

#' @name amlr_season
#' @export
amlr_date_from_season <- function(season.name, m, d) {
  sn.len <- length(season.name)
  .amlr_season_check(season.name)
  stopifnot(
    (sn.len == length(m)) | (length(m) == 1),
    (sn.len == length(d)) | (length(d) == 1)
  )

  # Make vectors the same length, for if_else below
  if (length(m) == 1) m <- rep(m, sn.len)
  if (length(d) == 1) d <- rep(d, sn.len)

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
    if_else(i[1] == "1999", "2000", paste0(str_sub(i[1], 1, 2), i[2]))
  }, "1")

  ymd(paste(if_else(m.num >= 7, season.name1, season.name2), m, d))
}

#' @name amlr_season
#' @export
amlr_year_from_season <- function(season.name) {
  sn.len <- length(season.name)
  .amlr_season_check(season.name)

  season1.int <- as.integer(str_sub(season.name, 1, 4))
  season2 <- str_sub(season.name, 6, 7)
  season.year <- case_when(
    season1.int >= 1999 ~ as.integer(paste0("20", season2)),
    season1.int < 1999 ~  as.integer(paste0("19", season2)),
    .default = NA_integer_
  )

  season.year
}


.amlr_season_check <- function(season.name) {
  # Checks:
  # 1) season name is 7 characters in length
  # 2) the separator is "/"
  # 3) the first season is from the 1900s or 2000s
  # 4) the passed years are sequential

  # Check 1
  if (any(nchar(season.name) != 7)) {
    stop("An AMLR season name must be exactly 7 characters, eg '2016/17'")
  } else {
    season1 <- str_sub(season.name, 1, 4)
    season2 <- str_sub(season.name, 6, 7)
    season.sep <- str_sub(season.name, 5, 5)

    # Check 2
    season.sep.expected <- c("/") # "-", "_")
    if (!all(season.sep %in% season.sep.expected))
      stop("The season separator is not one of the expected values: ",
           paste0("'", paste(season.sep.expected, collapse = "', '"), "'"))

    # Check 3
    if (any(as.integer(season1) < 1900))
      stop("The season name cannot be before 1900")

    # Check 4
    season2.expected <- case_when(
      season1 == "1999" ~ "00",
      .default = str_pad(as.integer(str_sub(season1, 3, 4)) + 1,
                         width = 2, pad = "0", side = "left")
    )

    if (any(season2 != season2.expected))
      stop("The season name must include sequential years, ",
           "such as '1999/00' or '2016/17'. ",
           "This season name does not contain sequential years. ",
           "Calendar year 1: ", season1, ", year 2: ", season2)
  }

  season.name
}
