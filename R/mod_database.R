#' Database module for AMLR shiny apps
#'
#' Database module for AMLR shiny apps
#'
#' @name mod_database
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param db.name.prod character; name of production database on SWFSC server
#' @param db.name.test character; name of test database on SWFSC server
#' @param remote_valid logical; indicates if the remote (TRUE) or local (FALSE)
#'   database connection should be the default
#' @param col.width integer; column width of column of UI widgets
#'
#' @export
mod_database_ui <- function(id, db.name.prod, db.name.test, remote_valid,
                            col.width = 5) {
  ns <- NS(id)

  choices.list <- list("remote_prod", "remote_test", "local_prod", "local_test")
  names(choices.list) <- c(
    paste(db.name.prod, "***REMOVED***", sep = " - "),
    paste(db.name.test, "***REMOVED***", sep = " - "),
    paste(db.name.prod, "local", sep = " - "),
    paste(db.name.test, "local", sep = " - ")
  )

  # assemble UI elements
  tagList(
    box(
      title = "Database connection information", status = "warning",
      solidHeader = FALSE, width = col.width, collapsible = TRUE,
      tableOutput(ns("pool_db_conn")),
      tags$br(),
      selectInput(ns("db_name"), tags$h5("Select database connection"), width = "300px",
                  choices = choices.list,
                  selected = if_else(remote_valid, "remote_prod", "local_prod"))
    )
  )
}

#' @name mod_database
#'
#' @param pool.remote.prod output of a \code{\link[pool]{dbPool}} call.
#'   A DBI database connection pool connected to the remote PRODUCTION database, e.g. '***REMOVED***'
#' @param pool.remote.test output of a \code{\link[pool]{dbPool}} call.
#'   A DBI database connection pool connected to the remote TEST database, e.g. '***REMOVED***_Test'
#' @param pool.local.prod output of a \code{\link[pool]{dbPool}} call.
#'   A DBI database connection pool connected to the local PRODUCTION database, e.g. '***REMOVED***'
#' @param pool.local.test output of a \code{\link[pool]{dbPool}} call.
#'   A DBI database connection pool connected to the local TEST database, e.g. '***REMOVED***_Test'
#' @param db.driver character; name of driver used to connect to remote database
#' @param db.server.remote character; name of server where remote database is hosted
#' @param db.server.local character; name of server where local database is hosted
#'
#' @returns \code{mod_database_server} returns a reactive of the pool connection specified by the user
#'
#' @export
mod_database_server <- function(id, db.name.prod, db.name.test,
                                pool.remote.prod, pool.remote.test,
                                pool.local.prod, pool.local.test,
                                db.driver, db.server.remote, db.server.local) {
  moduleServer(
    id,
    function(input, output, session) {
      vals.db <- reactiveValues(
        pool = NULL,
        db.name = "",
        system.user = ""
      )

      #----------------------------------------------------------------------------
      # Which database to use?
      observeEvent(input$db_name, {
        vals.db$pool <- if (input$db_name == "remote_prod") {
          pool.remote.prod
        } else if (input$db_name == "remote_test") {
          pool.remote.test
        } else if (input$db_name == "local_prod") {
          pool.local.prod
        } else if (input$db_name == "local_test") {
          pool.local.test
        } else {
          NULL
        }

        vals.db$db.name <- case_when(
          input$db_name %in% c("remote_prod", "local_prod") ~ db.name.prod,
          input$db_name %in% c("remote_test", "local_test") ~ db.name.test,
          TRUE ~ NA_character_
        )

        vals.db$system.user <- if (isTruthy(vals.db$pool)) {
          pool::dbGetQuery(req(vals.db$pool), "SELECT SYSTEM_USER")
        } else {
          NULL
        }
      })


      # Info about db connection
      output$pool_db_conn <- renderTable({
        validate(
          need(inherits(vals.db$pool, "Pool"),
               paste("The AMLR Shiny app was not able to connect to the specified database -",
                     "are you connected to VPN, or have you selected the correct local database?"))
        )

        server.curr <- case_when(
          input$db_name %in% c("remote_prod", "remote_test") ~ db.server.remote,
          input$db_name %in% c("local_prod", "local_test") ~ db.server.local,
          TRUE ~ NA_character_
        )

        data.frame(
          Label = c("Driver", "Server", "Username", "Database name"),
          Value = unlist(c(db.driver, server.curr, vals.db$system.user, vals.db$db.name))
        )
      })

      ### Return values
      return(reactive(vals.db$pool))
    }
  )
}
