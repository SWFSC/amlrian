#' ODBC Database Connections
#'
#' Wrappers around ODBC database connection functions,
#' with US AMLR default parameters
#'
#' @param Database character; name of database to connect to
#' @param Driver character; default is 'ODBC Driver 18 for SQL Server'
#' @param Server character; default is 'swc-***REMOVED***-s'
#' @param Trusted_Connection character; default is 'Yes'
#' @param Encrypt character; default is 'Optional'
#' @param idleTimeout integer; default is one hour
#' @param silent logical; default is \code{TRUE}.
#'   Passed as an argument to \code{\link[base]{try}}
#' @param ... additional arguments passed to \code{\link[pool]{dbPool}} or
#'   \code{\link[odbc]{dbConnect}}
#'
#' @name amlr_db
#"
#' @details
#' Wrapper for a call to \code{\link[pool]{dbPool}} or
#' \code{\link[odbc]{dbConnect}},
#' with an \code{\link[odbc]{odbc}} driver passed to \code{drv}.
#' See \code{\link[pool]{dbPool}} or \code{\link[odbc]{dbConnect}}
#' for more information about these arguments.
#'
#' Arguments that are \code{NULL} are ignored and not passed to
#' the database connection function. This allows you to, for instance pass a
#' username and password rather than Trusted_Connection string (see examples).
#'
#' @return
#' Output of \code{\link[base]{try}(\link[pool]{dbPool}(..), silent = TRUE)}, or
#' \code{\link[base]{try}(\link[odbc]{dbConnect}(..), silent = TRUE)}
#'
#' @examples
#' \dontrun{
#' amlr_dbPool("***REMOVED***")
#' amlr_dbConnect("***REMOVED***")
#'
#' # Connect using username and password
#' amlr_dbPool(
#'   "***REMOVED***",
#'   Trusted_Connection = NULL, uid = "sa", pwd = "SecurePwd"
#' )
#' }
#'
#' @seealso \url{https://github.com/rstudio/pool}
#'
#' @export
amlr_dbPool <- function(Database,
                        Driver = "ODBC Driver 18 for SQL Server",
                        Server = "swc-***REMOVED***-s",
                        Trusted_Connection = "Yes",
                        Encrypt = "Optional",
                        idleTimeout = 3600000,
                        silent = TRUE,
                        ...) {
  stopifnot(
    inherits(silent, "logical")
  )
  .driver_check(Driver)

  # https://stackoverflow.com/questions/11885207/get-all-parameters-as-list
  db.list <- c(drv = odbc::odbc(), as.list(environment()), list(...))

  try(do.call(pool::dbPool, purrr::compact(db.list)), silent = silent)
}


#' @name amlr_db
#' @export
amlr_dbConnect <- function(Database,
                           Driver = "ODBC Driver 18 for SQL Server",
                           Server = "swc-***REMOVED***-s",
                           Trusted_Connection = "Yes",
                           Encrypt = "Optional",
                           silent = TRUE,
                           ...) {
  stopifnot(
    inherits(silent, "logical")
  )
  .driver_check(Driver)

  # https://stackoverflow.com/questions/11885207/get-all-parameters-as-list
  db.list <- c(drv = odbc::odbc(), as.list(environment()), list(...))

  try(do.call(odbc::dbConnect, purrr::compact(db.list)), silent = silent)
}


# Check driver
.driver_check <- function(Driver) {
  driver.rec <- "ODBC Driver 18 for SQL Server"
  if (Driver != driver.rec)
    warning("You are not connecting with the recommended driver: ", driver.rec,
            immediate. = TRUE)
}
