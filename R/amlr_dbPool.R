#' Wrapper with US AMLR defaults around dbPool
#'
#' Wrapper around dbPool with standard, US AMLR defaults for connecting to
#' an AMLR database with an ODBC driver
#'
#' @param Database character; name of database to connect to
#' @param Driver character; default is 'ODBC Driver 18 for SQL Server'
#' @param Server character; default is 'swc-***REMOVED***-s'
#' @param Trusted_Connection character; default is 'Yes'
#' @param Encrypt character; default is 'Optional'
#' @param idleTimeout integer; default is one hour
#' @param silent logical; default is \code{TRUE}.
#'   Passed to silent argument of \code{\link[base]{try}}
#' @param ... additional arguments passed to \link[pool]{dbPool}
#'
#' @details
#' Wrapper for a call to \code{\link[pool]{dbPool}},
#' with an \code{\link[odbc]{odbc}} driver passed to \code{drv}.
#' See \code{\link[pool]{dbPool}} for more information about these arguments.
#'
#' Arguments that are \code{NULL} are ignored and not passed to
#' \code{\link[pool]{dbPool}}. This allows you to, for instance pass a
#' username and password rather than Trusted_Connection string (see examples).
#'
#' @return
#' Output of a \code{\link[base]{try}(\link[pool]{dbPool}(..), silent = TRUE)}
#'
#' @examples
#' \dontrun{
#' amlr_dbPool("***REMOVED***")
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

  # https://stackoverflow.com/questions/11885207/get-all-parameters-as-list
  db.list <- c(drv = odbc::odbc(), as.list(environment()), list(...))

  try(do.call(pool::dbPool, purrr::compact(db.list)), silent = silent)
}
