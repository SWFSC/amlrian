#' Open the amlrDatabases Shiny app
#'
#' Open the amlrDatabases Shiny app
#'
#' @param ... arguments passed as a list to the \code{options} argument
#'   of \code{\link[shiny]{runApp}}
#'
#' @examples
#' if (interactive()) amlrDatabases_shiny(launch.browser = TRUE)
#'
#' @export
amlrDatabases_shiny <- function(...) {
  ##############################################################################
  # Set connections to remote dbs
  db.driver <- "ODBC Driver 18 for SQL Server"
  db.server.remote <- "swc-***REMOVED***-s"

  db.name.prod <- "***REMOVED***"
  db.name.test <- "***REMOVED***_Test"

  pool.remote.prod <- amlr_dbPool(db.name.prod, db.driver, db.server.remote)
  remote.prod.valid <- isTruthy(pool.remote.prod)

  if (remote.prod.valid) {
    pool.remote.test <- amlr_dbPool(db.name.test, db.driver, db.server.remote)
    remote.prod.valid <- dbIsValid(pool.remote.prod)
  } else {
    pool.remote.test <- NULL
  }

  onStop(function() {
    if (isTruthy(pool.remote.prod))
      if (dbIsValid(pool.remote.prod)) poolClose(pool.remote.prod)
    if (isTruthy(pool.remote.test))
      if (dbIsValid(pool.remote.test)) poolClose(pool.remote.test)
  })


  ##############################################################################
  # Shiny app
  ui <- dashboardPage(
    title = "Testing amlrDatabases",
    dashboardHeader(title = "Testing amlrDatabases"),

    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Database module", tabName = "tab_database",
                 icon = icon("th", lib = "font-awesome")),
        menuItem("Output module", tabName = "tab_output",
                 icon = icon("th", lib = "font-awesome"))
        # actionButton("stop", "Close App")
      ), width = "230"
    ),

    dashboardBody(
      tags$head(tags$style(HTML("
      .shiny-output-error-validation {
      color: red; font-weight: bold;
      }
    "))),
    tabItems(
      tabItem(
        "tab_database",
        mod_database_ui("db", col.width = 12)
      ),
      tabItem(
        "tab_output",
        box(
          width = 7,
          tags$h5("To double check database connectivity, specify a ",
                  "table name and click 'Fetch' to print the ",
                  "head() of the table. ",
                  tags$br(), tags$br(),
                  "Note that tables may have columns that can't be printed; ",
                  "these columns may cause an error if not removed."),
          fluidRow(
            column(6, textInput("tbl_name", tags$h5("Table name"))),
            column(6, tags$br(), tags$br(), actionButton("tbl_go", "Fetch"))
          ),
          fluidRow(
            column(6, uiOutput("tbl_x_uiOut")),
            column(6, uiOutput("tbl_y_uiOut"))
          )
        ),
        mod_output_ui("tbl_test")
      )
    )
    )
  )

  server <- function(input, output, session) {
    session$onSessionEnded(function() {
      # Close current pool object. Needed here in case working off 'other' db
      isolate({
        if (inherits(db.pool(), "Pool")) {
          if (dbIsValid(db.pool())) {
            poolClose(db.pool())
          }
        }
      })
      stopApp(returnValue = "Shiny app was closed")
    })

    #--------------------------------------------------------
    pool.list <- list(
      `***REMOVED*** - ***REMOVED***` = pool.remote.prod,
      `***REMOVED***_Test - ***REMOVED***` = pool.remote.test
    )
    db.pool <- mod_database_server(
      "db", pool.list, db.driver
      # db.selected = "bobb"
    )


    #--------------------------------------------------------
    tbl_tbl <- eventReactive(input$tbl_go, {
      x <- try(tbl(db.pool(), input$tbl_name) %>% collect(), silent = TRUE)
      validate(need(x, "Unable to fetch table"))

      x[, setdiff(names(x), "ts")]
    })

    output$tbl_x_uiOut <- renderUI({
      selectInput("tbl_x", tags$h5("X axis variable"),
                  choices = names(tbl_tbl()))
    })

    output$tbl_y_uiOut <- renderUI({
      selectInput("tbl_y", tags$h5("Y axis variable"),
                  choices = names(tbl_tbl()),
                  selected = names(tbl_tbl())[2])
    })

    tbl_plot <- reactive({
      x <- req(tbl_tbl())
      validate(
        need(!identical(input$tbl_x, input$tbl_y),
             "Please choose different variables for x and y")
      )

      ggplot(x, aes(!!as.name(input$tbl_x), !!as.name(input$tbl_y))) +
        geom_point() +
        ggtitle("Questionable plot")

    })

    observe(mod_output_server("tbl_test", tbl_tbl, tbl_plot))
  }

  shiny::shinyApp(ui = ui, server = server, options = list(...))
}
