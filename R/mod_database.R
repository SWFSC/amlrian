#' Database module for AMLR shiny apps
#'
#' Database module for AMLR shiny apps
#'
#' @name mod_database
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param db.name.prod character; name of production database on SWFSC server
#' @param db.name.test character; name of test database on SWFSC server
#' @param remote.valid logical; indicates if the remote (TRUE) or local (FALSE)
#'   database connection should be the default
#' @param col.width integer; column width of column of UI widgets
#' @param db.remote.default character; default remote database to connect to.
#'   To allow developer to specify test database as initial db
#'
#' @export
mod_database_ui <- function(id, db.name.prod, db.name.test, remote.valid,
                            col.width = 5, db.remote.default = "remote_prod") {
  ns <- NS(id)

  choices.list <- list("remote_prod", "remote_test", "other")
  stopifnot(db.remote.default %in% choices.list)
  names(choices.list) <- c(
    paste(db.name.prod, "- ***REMOVED***"),
    paste(db.name.test, "- ***REMOVED***"),
    "other"
  )
  local.server <- paste0(Sys.info()[["nodename"]], "\\SQLEXPRESS")

  # assemble UI elements
  tagList(
    box(
      title = "Database connection information", status = "warning",
      solidHeader = FALSE, width = col.width, collapsible = TRUE,
      tableOutput(ns("pool_db_conn")),
      tags$br(),
      radioButtons(ns("db_name"), tags$h5("Select database connection"),
                   choices = choices.list,
                   selected = if_else(remote.valid, db.remote.default, "other")),
      conditionalPanel(
        condition = "input.db_name == 'other'", ns = ns,
        box(
          width = 12,
          textInput(ns("db_other_server"), tags$h5("SQL Server Name"),
                    value = local.server),
          textInput(ns("db_other_database"), tags$h5("SQL Server Database"),
                    value = db.name.prod),
          actionButton(ns("db_other_action"), "Connect to database")
        )
      )
    )
  )
}

#' @name mod_database
#'
#' @param pool.remote.prod output of a \code{\link[pool]{dbPool}} call.
#'   A DBI database connection pool connected to the remote PRODUCTION database,
#'   e.g. '***REMOVED***'
#' @param pool.remote.test output of a \code{\link[pool]{dbPool}} call.
#'   A DBI database connection pool connected to the remote TEST database,
#'   e.g. '***REMOVED***_Test'
#' @param db.driver character; name of driver used to connect to the databases
#'
#' @details
#' This module allows users to connect to the database of their choice:
#' the provided production database, the provided dev (test) database,
#' or a database specified by the user (e.g., a local database).
#'
#' If the user specifies their own database,
#' then the pool object is created and returned by this module.
#'
#' @returns
#' \code{mod_database_server} returns a reactive of the pool connection
#' specified by the user
#'
#' @export
mod_database_server <- function(id, pool.remote.prod, pool.remote.test, db.driver) {
  moduleServer(
    id,
    function(input, output, session) {
      vals.db <- reactiveValues(
        pool = NULL,
        server.name = "",
        db.name = "",
        system.user = "",
        other = FALSE
      )

      #----------------------------------------------------------------------------
      # Which database to use?
      db_listen <- reactive({
        list(input$db_name, input$db_other_action)
      })

      observeEvent(db_listen(), {
        if (isTruthy(vals.db$pool) & vals.db$other) {
          if (pool::dbIsValid(vals.db$pool)) pool::poolClose(vals.db$pool)
        }

        vals.db$pool <- if (input$db_name == "remote_prod") {
          pool.remote.prod
        } else if (input$db_name == "remote_test") {
          pool.remote.test
        } else if (input$db_name == "other") {
          amlr_dbPool(Driver = db.driver, Server = input$db_other_server,
                      Database = input$db_other_database)
        } else {
          NULL
        }

        vals.db$other <- input$db_name == "other"
      })


      # Get and print info about db connection
      output$pool_db_conn <- renderTable({
        validate(
          need(inherits(vals.db$pool, "Pool"),
               paste("Tamatoa was unable to connect to the specified database.",
                     "Are you connected to VPN, and/or have you",
                     "specified the correct server and database names?"))
        )

        db.query <- pool::dbGetQuery(
          req(vals.db$pool), "SELECT @@servername, DB_NAME(), SYSTEM_USER"
        )

        vals.db$server.name <- db.query[[1]]
        vals.db$db.name <- db.query[[2]]
        vals.db$system.user <- db.query[[3]]

        data.frame(
          Label = c("Driver", "Server", "Database", "User"),
          Value = unlist(
            c(db.driver, vals.db$server.name, vals.db$db.name, vals.db$system.user)
          )
        )
      })

      ### Return values
      return(reactive(vals.db$pool))
    }
  )
}
