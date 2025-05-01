#' Database connection module for AMLR shiny apps
#'
#' Database connection module for AMLR shiny apps
#'
#' @name mod_database
#'
#' @param id module namespace, see [shiny::NS()]
#' @param col.width integer; column width of column of UI widgets
#' @param server.default default character value for "Server Hostname" widget;
#'   default is `NULL`. If `NULL`, the module defaults to
#'   `paste0(Sys.info()[["nodename"]], "\\SQLEXPRESS")`
#' @param database.default default character value for "Database" widget;
#'   default is `NULL`
#' @param port.default default numeric value for "Port number" widget; default
#'   is `NULL`
#' @param conn.default default character value for "Connection type" widget;
#'   default is `"trusted"`
#' @param uid.default default character value for "User" widget; default is
#'   `NULL`
#' @param pwd.default default character value for "Password" widget; default is
#'   `NULL`
#'
#' @export
mod_database_ui <- function(id,
                            col.width = 5,
                            server.default = NULL,
                            database.default = NULL,
                            port.default = NULL,
                            conn.default = "trusted",
                            uid.default = NULL,
                            pwd.default = NULL) {
  ns <- NS(id)

  if (is.null(server.default))
    server.default <- paste0(Sys.info()[["nodename"]], "\\SQLEXPRESS")

  port.check.default <- !is.null(port.default)
  drivers.list <- odbc::odbcListDrivers() %>%
    filter(attribute == "SQLLevel", value > 0) %>%
    pull(name) %>%
    unique()

  if (!(amlrDatabases::amlr.driver %in% drivers.list))
    warning("The preferred driver ('", amlrDatabases::amlr.driver,
            "') is not available on this machine")

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
          fluidRow(
            column(6, textInput(ns("db_other_server"), tags$h5("Server hostname"),
                                value = server.default)),
            column(6, textInput(ns("db_other_database"), tags$h5("Database"),
                                value = database.default)),
            column(6, selectInput(ns("db_other_driver"), tags$h5("Driver"),
                                  choices = drivers.list,
                                  selected = amlrDatabases::amlr.driver))
          ),
          fluidRow(
            column(6, checkboxInput(ns("db_other_port_check"),
                                    "Specify port number",
                                    value = port.check.default)),
            column(
              width = 6,
              conditionalPanel(
                condition = "input.db_other_port_check == true", ns = ns,
                numericInput(ns("db_other_port"), tags$h5("Port number"),
                             value = port.default)
              )
            )
          ),
          radioButtons(ns("db_other_conn"), tags$h5("Connection type"),
                       choices = c("Trusted connection" = "trusted",
                                   "User login" = "login"),
                       selected = conn.default),
          conditionalPanel(
            condition = "input.db_other_conn == 'login'", ns = ns,
            fluidRow(
              column(6, textInput(ns("db_other_uid"), tags$h5("User"),
                                  value = uid.default)),
              column(6, textInput(ns("db_other_pwd"), tags$h5("Password"),
                                  value = pwd.default))
            )
          ),
          actionButton(ns("db_other_action"), "Connect to other database")
        )
      )
    )
  )
}

#' @name mod_database
#'
#' @param pool.list a (named) list of pool objects created by [pool::dbPool()].
#'   The names of the objects in this list will appear as
#'   'database connection' [shiny::radioButtons()] options.
#' @param filedsn character; default is `NULL.` The file path to a DSN file with a
#'   database connection. If not `NULL`, Tamatoa will try to try to establish a
#'   database connection using [pool::dbPool]. See 'Details' for an example
#'
#' @details
#' This module includes the UI for a user to specify and
#' create a database connection. It also allows users to connect to the database
#' of their choice in a parent function, and pass that connection (those
#' connections) via `pool.list`. Additionally, users can provide a filedsn,
#' and the module will attempt to create the connection defined by the `filedsn`
#' argument.
#'
#' If `filedsn` argument is not `NULL`, then Tamatoa will try to make a
#' connection via:
#'
#' `pool::dbPool(odbc::odbc(), filedsn = filedsn)`
#'
#' The order of priority for the default selected database: `pool.list`,
#' `filedsn`, then 'Other'.
#'
#' @returns
#' Returns a reactive of the pool connection specified by the user, which can
#' then be passed to other modules.
#'
#' @export
mod_database_server <- function(id, pool.list = list(), filedsn = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      vals.db <- reactiveValues(
        pool = NULL,
        other = FALSE
      )

      # Connect to database specified by filedsn argument, if specified
      if (!is.null(filedsn)) {
        pool.filedsn <- try(pool::dbPool(odbc::odbc(), filedsn = filedsn),
                            silent = TRUE)

        if (isTruthy(pool.filedsn)) {
          onStop(function() {
            if (isTruthy(pool.filedsn))
              if (dbIsValid(pool.filedsn)) {
                print("closing2")
                poolClose(pool.filedsn)
              }
          })

          pool.list <- c(pool.list, `filedsn argument` = pool.filedsn)

        } else {
          warning("Unable to make a database connection using ",
                  "the provided filedsn. Connection will be ignored",
                  immediate. = TRUE)
        }
      }


      #----------------------------------------------------------------------------
      ### Render the widget to select the Database connection
      output$db_conn_uiOut <- renderUI({
        choices.list <- c(names(pool.list), "other")
        names(choices.list) <- c(names(pool.list), "Other")

        radioButtons(
          session$ns("db_conn"), "Select database connection",
          choices = choices.list
        )
      })

      #----------------------------------------------------------------------------
      #

      ### Close other database
      db_other_close <- function() {
        if (isTruthy(vals.db$pool) && vals.db$other) {
          if (dbIsValid(vals.db$pool)) {
            print("closing1")
            poolClose(vals.db$pool)
            vals.db$pool <- NULL
          }
        }
      }

      ### Default databases
      observeEvent(input$db_conn, {
        req(input$db_conn != "other")
        db_other_close()

        vals.db$other <- FALSE
        vals.db$pool <- pool.list[[req(input$db_conn)]]
      })

      ### Connect to an 'other' database, on button click
      observeEvent(input$db_other_action, {
        db_other_close()

        db.driver <- input$db_other_driver
        encrypt.driver <- "ODBC Driver 18 for SQL Server"
        trusted.connection <- if_else(db.driver == encrypt.driver, "Yes", "TRUE")
        trusted.connection.no <- if_else(db.driver == encrypt.driver, "No", "FALSE")


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

        con <- pool::localCheckout(vals.db$pool)
        info <- pool::dbGetInfo(con)

        data.frame(
          Label = c("Server", "Database", "User", "Driver", "Driver Version"),
          Value = unlist(info[c("servername", "dbname", "username",
                                "drivername", "driver.version")])
        )
      })

      ### Return values
      return(reactive(vals.db$pool))
    }
  )
}
