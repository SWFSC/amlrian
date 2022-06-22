library(amlrDatabases)
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(pool)

###############################################################################
# Set connections to remote dbs
db.driver <- "ODBC Driver 18 for SQL Server"
db.server.remote <- "swc-***REMOVED***-s"

db.name.prod <- "***REMOVED***"
db.name.test <- "***REMOVED***_Test"

pool.remote.prod <- amlrDatabases::amlr_dbPool(db.name.prod, db.driver, db.server.remote)
remote.prod.valid <- isTruthy(pool.remote.prod)

if (remote.prod.valid) {
  pool.remote.test <- amlrDatabases::amlr_dbPool(db.name.test, db.driver, db.server.remote)
  remote.prod.valid <- DBI::dbIsValid(pool.remote.prod)
} else {
  pool.remote.test <- NULL
}


onStop(function() {
  if (isTruthy(pool.remote.prod))
    if (pool::dbIsValid(pool.remote.prod)) poolClose(pool.remote.prod)
  if (isTruthy(pool.remote.test))
    if (pool::dbIsValid(pool.remote.test)) poolClose(pool.remote.test)
})


################################################################################
# Shiny app
ui <- dashboardPage(
  title = "Testing amlrDatabases",
  dashboardHeader(title = "Testing amlrDatabases"),

  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Database Info", tabName = "tab_info", icon = icon("th", lib = "font-awesome"))
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
        "tab_info",
        # fluidRow(
        mod_database_ui("db", db.name.prod, db.name.test, remote.prod.valid,
                        db.remote.default = "remote_test",
                        col.width = 12)
        # box(
        #   width = 7,
        #   tags$h5("To double check database connectivity, specify a ",
        #           "table name and click 'Fetch' to print the ",
        #           "head() of the table. ",
        #           tags$br(), tags$br(),
        #           "Note that some tables may have columns that can't be printed; ",
        #           "these columns may be removed or cause an error. "),
        #   fluidRow(
        #     column(6, textInput("tbl_name", tags$h5("Table name"))),
        #     column(6, tags$br(), tags$br(), actionButton("tbl_go", "Fetch"))
        #   ),
        #   DTOutput("tbl_out")
        # )
        # )
      )
    )
  )
)

server <- function(input, output, session) {
  session$onSessionEnded(function() {
    # Close current pool object. Needed here in case working off 'other' db
    isolate({
      if (inherits(db_pool(), "Pool")) {
        if (pool::dbIsValid(db_pool())) {
          pool::poolClose(db_pool())
        }
      }
    })
    stopApp(returnValue = "Shiny app was closed")
  })

  db_pool <- mod_database_server(
    "db", pool.remote.prod, pool.remote.test, db.driver
  )

  # tbl_tbl <- eventReactive(input$tbl_go, {
  #   x <- try(
  #     tbl(db_pool(), input$tbl_name) %>% collect() %>% head(),
  #     silent = TRUE
  #   )
  #   validate(need(x, "Unable to fetch table"))
  #
  #
  #   if ("ts" %in% names(x)) x %>% select(-ts) else x
  # })
  #
  # output$tbl_out <- renderDT({
  #   tbl_tbl()
  # })
}

shiny::shinyApp(ui = ui, server = server)
