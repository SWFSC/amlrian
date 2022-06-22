#' Open the amlrDatabases Shiny app
#'
#' Open the amlrDatabases Shiny app (for testing)
#'
#' @param launch.browser Logical with default of \code{TRUE};
#'   passed to \code{launch.browser} argument of \code{\link[shiny]{runApp}}
#'
#' @examples
#' if (interactive()) tamatoa(launch.browser = TRUE)
#'
#' @seealso \url{https://www.fisheries.noaa.gov/about/antarctic-ecosystem-research-division-southwest-fisheries-science-center}
#'
#' @export
amlrDatabases_shiny <- function(launch.browser = TRUE) {
  appDir <- system.file("shiny", package = "amlrDatabases")
  if (appDir == "") {
    stop("There was an error opening the amlrDatabases Shiny app; try re-installing 'amlrDatabases'",
         call. = FALSE)
  }
  shiny::runApp(appDir, launch.browser = launch.browser, display.mode = "normal")
}
