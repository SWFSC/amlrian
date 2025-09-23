#' ODBC Database Connections
#'
#' Wrappers around ODBC database connection functions,
#' with US AMLR default parameters
#'
#' @param Database character; name of database to connect to
#' @param Server character; name of the database server
#' @param Driver character; default is 'ODBC Driver 18 for SQL Server'
#' @param Trusted_Connection character; default is 'Yes'
#' @param Encrypt character; default is 'Optional'
#' @param idleTimeout integer; default is one hour
#' @param silent logical; default is `TRUE`.
#'   Passed as an argument to [base::try()]
#' @param ... additional arguments passed to [pool::dbPool()] or
#'   [odbc::dbConnect()]
#'
#' @name amlr_db
#"
#' @details Wrapper for a call to [pool::dbPool()] or [odbc::dbConnect()], with
#' the [odbc::odbc()] driver passed to `drv`. See the above-linked
#' functions for more information about these arguments.
#'
#' Arguments that are `NULL` are ignored and not passed to the database
#' connection function. This allows you to, for instance pass a username and
#' password rather than Trusted_Connection string (see examples).
#'
#' @return
#' Output of \code{\link[base]{try}(\link[pool]{dbPool}(..), silent = TRUE)}, or
#' \code{\link[base]{try}(\link[odbc]{dbConnect}(..), silent = TRUE)}
#'
#' @examples
#' \dontrun{
#' db.name <- "db-name"
#' server.name <- "server-name"
#' amlr_dbPool(Database = db.name, Server = server.name)
#' amlr_dbConnect(Database = db.name, Server = server.name)
#'
#' # Connect using username and password
#' amlr_dbPool(
#'   Database, Server,
#'   Trusted_Connection = NULL, uid = "username", pwd = "SecurePwd"
#' )
#' }
#'
#' @seealso \url{https://github.com/rstudio/pool}
#'
#' @export
amlr_dbPool <- function(Database, Server,
                        Driver = "ODBC Driver 18 for SQL Server",
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
amlr_dbConnect <- function(Database, Server,
                           Driver = "ODBC Driver 18 for SQL Server",
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
  if (Driver != amlrian::amlr.driver)
    warning("You are not connecting with the AMLR-preferred driver: ",
            amlrian::amlr.driver, immediate. = TRUE)
}
