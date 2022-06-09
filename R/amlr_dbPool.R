#' Wrapper to connect to an AMLR database
#'
#' Wrapper with standard, AMLR defaults for connecting to an AMLR database
#'
#' @param Database character; database name
#' @param Driver character; default is 'ODBC Driver 18 for SQL Server'
#' @param Server character; default is 'swc-***REMOVED***-s'
#' @param Trusted_Connection character; default is 'Yes'
#' @param Encrypt character; default is 'Optional'
#' @param idleTimeout integer; default is one hour
#' @param ... additional arguments passed to \link[pool]{dbPool}
#'
#' @details
#' Wrapper for a call to \code{\link[pool]{dbPool}}.
#' See these docs for more information about these arguments
#'
#' @return
#' Output of \code{\link[base]{try}(\link[pool]{dbPool})} call
#'
#' @examples
#' \dontrun{
#' amlr_dbPool("***REMOVED***")
#' }
#'
#' @seealso https://github.com/rstudio/pool
#'
#' @export
amlr_dbPool <- function(Database, Driver = "ODBC Driver 18 for SQL Server",
                        Server = "swc-***REMOVED***-s", Trusted_Connection = "Yes",
                        Encrypt = "Optional", idleTimeout = 3600000, ...) {
  try(pool::dbPool(
    drv = odbc::odbc(),
    Driver = Driver,
    Server = Server,
    Database = Database,
    Trusted_Connection = Trusted_Connection,
    Encrypt = Encrypt,
    idleTimeout = idleTimeout, #3600000  # 1 hour
    ...
  ), silent = TRUE)
}
