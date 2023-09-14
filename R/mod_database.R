#' Database module for AMLR shiny apps
#'
#' Database module for AMLR shiny apps
#'
#' @name mod_database
#'
#' @param id module namespace, see \code{shiny::\link[shiny]{NS}}
#' @param col.width integer; column width of column of UI widgets
#' @param server.default default character value for "SQL Server Name";
#'   default is \code{NULL}. If \code{NULL}, default in module is
#'   \code{paste0(\link{Sys.info}()[["nodename"]], "\\SQLEXPRESS")}
#' @param database.default default character value for "SQL Server Database" widget;
#'   default is "***REMOVED***"
#' @param port.check.default default boolean value for 'Specify port number' widget;
#'   default is \code{FALSE}
#' @param port.default default numeric value for "Port number";
#'   default is ***REMOVED***
#' @param conn.default default character value for "Connection type";
#'   default is "trusted
#' @param uid.default default character value for "User"; default is "sa"
#' @param pwd.default default character value for "Password";
#'   default is \code{NULL}
#' @export
mod_database_ui <- function(id,
                            col.width = 5,
                            server.default = NULL,
                            database.default = "***REMOVED***",
                            port.check.default = FALSE,
                            port.default = ***REMOVED***,
                            conn.default = "trusted",
                            uid.default = "sa",
                            pwd.default = NULL) {
  ns <- NS(id)

  if (is.null(server.default))
    server.default <- paste0(Sys.info()[["nodename"]], "\\SQLEXPRESS")

  # assemble UI elements
  tagList(
    box(
      title = "Database connection information", status = "warning",
      solidHeader = FALSE, width = col.width, collapsible = TRUE,
      tableOutput(ns("pool_db_conn")),
      tags$br(),
      uiOutput(ns("db_conn_uiOut")),
      conditionalPanel(
        condition = "input.db_conn == 'other'", ns = ns,
        box(
          width = 12,
          textInput(ns("db_other_server"), tags$h5("SQL Server Name"),
                    value = server.default),
          textInput(ns("db_other_database"), tags$h5("SQL Server Database"),
                    value = database.default),
          checkboxInput(ns("db_other_port_check"), "Specify port number",
                        value = port.check.default),
          conditionalPanel(
            condition = "input.db_other_port_check == true", ns = ns,
            numericInput(ns("db_other_port"), tags$h5("Port number"),
                         value = port.default)
          ),
          radioButtons(ns("db_other_conn"), tags$h5("Connection type"),
                       choices = c("Trusted connection" = "trusted",
                                   "User login" = "login"),
                       selected = conn.default),
          conditionalPanel(
            condition = "input.db_other_conn == 'login'", ns = ns,
            textInput(ns("db_other_uid"), tags$h5("User"), value = uid.default),
            textInput(ns("db_other_pwd"), tags$h5("Password"), value = pwd.default)
          ),
          actionButton(ns("db_other_action"), "Connect to other database")
        )
      )
    )
  )
}

#' @name mod_database
#'
#' @param pool.list a (named) list of pool objects created by \code{\link[pool]{dbPool}}.
#'   The names of the objects in this list will appear as
#'   'database connection' \code{\link[shiny]{radioButtons}} options.
#' @param db.driver character; name of driver used to connect to the databases
#' @param db.selected character; the initial selected value for the
#'   database connection widget (\code{output$db_conn_uiOut}).
#'   Passed to \code{selected} argument of \code{\link[shiny]{radioButtons}}.
#'   Must be \code{NULL}, or the name of a connection in \code{pool.list}
#'
#' @details
#' This module allows users to connect to the database of their choice:
#' any already-created connections
#' (e.g., to the ***REMOVED*** production database on the SWC server)
#' or a database specified by the user.
#'
#' If the user specifies their own database,
#' then this module creates the pool object.
#'
#' @returns
#' Returns a reactive of the pool connection specified by the user
#'
#' @export
mod_database_server <- function(id, pool.list = list(),
                                db.driver = "ODBC Driver 18 for SQL Server",
                                db.selected = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      vals.db <- reactiveValues(
        pool = NULL,
        other = FALSE
      )

      encrypt.driver <- "ODBC Driver 18 for SQL Server"
      trusted.connection <- if_else(db.driver == encrypt.driver, "Yes", "TRUE")
      trusted.connection.no <- if_else(db.driver == encrypt.driver, "No", "FALSE")


      #----------------------------------------------------------------------------
      # Connect to database

      ### Close other database
      db_other_close <- function() {
        if (isTruthy(vals.db$pool) && vals.db$other) {
          if (dbIsValid(vals.db$pool)) {
            poolClose(vals.db$pool)
            vals.db$pool <- NULL
          }
        }
      }

      ### Database connection widget based on input
      output$db_conn_uiOut <- renderUI({
        validate(
          need(db.selected %in% names(pool.list),
                "db.selected is not a named item in pool.list")
        )
        choices.list <- c(pool.list, "other")
        names(choices.list) <- c(names(pool.list), "Other")

        radioButtons(
          session$ns("db_conn"), tags$h5("Select database connection"),
          choices = choices.list, selected = db.selected
        )
      })

      ### Default databases
      observeEvent(input$db_conn, {
        req(input$db_conn != "other")
        db_other_close()

        vals.db$other <- FALSE
        vals.db$pool <- pool.list[[req(input$db_conn)]]
      })

      ### Other database, on button click
      observeEvent(input$db_other_action, {
        db_other_close()

        db.other.conn <- if (input$db_other_conn == "trusted") {
          list(Trusted_Connection = trusted.connection)
        } else if (input$db_other_conn == "login") {
          list(uid = input$db_other_uid, pwd = input$db_other_pwd,
               Trusted_Connection = trusted.connection.no)
        } else {
          stop("invalid input$db_other_conn value")
        }

        db.args.list <- c(
          list(
            Driver = db.driver,
            Server = input$db_other_server,
            Database = input$db_other_database,
            port = if (input$db_other_port_check) input$db_other_port else NULL,
            Encrypt = if (db.driver == encrypt.driver) "Optional" else NULL
          ),
          db.other.conn
        )

        vals.db$other <- TRUE
        vals.db$pool <- do.call(amlr_dbPool, purrr::compact(db.args.list))
      })


      #----------------------------------------------------------------------------
      # Outputs

      ### Get and print info about db connection
      output$pool_db_conn <- renderTable({
        validate(
          need(inherits(vals.db$pool, "Pool"),
               paste("The Shiny app was unable to connect to the specified database.",
                     "Are you connected to VPN, and/or have you",
                     "specified the correct connection arguments?"))
        )

        db.query <- dbGetQuery(
          vals.db$pool, "SELECT @@servername, DB_NAME(), SYSTEM_USER"
        )

        data.frame(
          Label = c("Driver", "Server", "Database", "User"),
          Value = unlist(
            c(db.driver, db.query[[1]], db.query[[2]], db.query[[3]])
          )
        )
      })

      ### Return values
      return(reactive(vals.db$pool))
    }
  )
}
