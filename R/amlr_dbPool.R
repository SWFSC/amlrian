#' Wrapper to connect to an AMLR database
#'
#' Wrapper with standard, AMLR defaults for connecting to an AMLR database
#'
#' @param Database character; see \code{\link[pool]{dbPool}}
#' @param Driver character; default is 'SQL Server'. See \code{\link[pool]{dbPool}}
#' @param Server character; default is 'swc-***REMOVED***-s'. See \code{\link[pool]{dbPool}}
#' @param idleTimeout integer; default is one hour. See \code{\link[pool]{dbPool}}
#' @param ... additional arguments passed to \link[pool]{dbPool})
#'
#' @details Wrapper for a call to \link[pool]{dbPool})
#'
#' @return Output of \code{\link[base]{try}(\link[pool]{dbPool})} call
#'
#' @examples
#' \dontrun{
#' amlr_dbPool("***REMOVED***")
#' }
#'
#' @seealso https://github.com/rstudio/pool
#'
#' @export
amlr_dbPool <- function(Database, Driver = "SQL Server", Server = "swc-***REMOVED***-s",
                        idleTimeout = 3600000, ...) {
  try(pool::dbPool(
    drv = odbc::odbc(),
    Driver = Driver,
    Server = Server,
    Database = Database,
    Trusted_Connection = "True",
    idleTimeout = idleTimeout, #3600000  # 1 hour
    ...
  ), silent = TRUE)
}
