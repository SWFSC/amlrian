#' Open the package test Shiny app
#'
#' Open the package test Shiny app
#'
#' @param ... arguments passed as a list to the `options` argument of
#'   [shiny::runApp()]
#' @param filedsn character; see [mod_database()] for details
#'
#' @details
#' The Shiny app is a testing/development app. To date, is has
#' been used to develop and test the database connection and output modules
#' contained in this package
#'
#' @examplesIf interactive()
#' shiny_test(launch.browser = TRUE)
#'
#' @export
shiny_test <- function(..., filedsn = NULL) {
  ##### Prep work
  # NOTE: yes, this intentionally duplicates the filedsn connection
  # Left as an example
  if (!is.null(filedsn)) {
    pool.filedsn <- pool::dbPool(odbc::odbc(), filedsn = filedsn)

    onStop(function() {
      if (isTruthy(pool.filedsn))
        if (dbIsValid(pool.filedsn)) poolClose(pool.filedsn)
    })
  }


  ##### Shiny app
  ui <- dashboardPage(
    title = "Dev shiny app",
    dashboardHeader(title = "Dev shiny app"),

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
          fluidRow(mod_database_ui("db", col.width = 6))
        ),
        tabItem(
          "tab_output",
          fluidRow(
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
              conditionalPanel(
                condition = "output.tbl_flag",
                fluidRow(
                  column(4, checkboxInput("txt_display", "Display text")),
                  column(4, uiOutput("tbl_x_uiOut")),
                  column(4, uiOutput("tbl_y_uiOut"))
                )
              )
            )
          ),
          mod_output_ui("out_test")
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
    pool.list <- purrr::compact(list(
      `filedsn argument og` = if (isTruthy(filedsn)) pool.filedsn else NULL
    ))
    db.pool <- mod_database_server("db", pool.list, filedsn = filedsn)


    output$tbl_flag <- reactive(isTruthy(out_tbl()))
    outputOptions(output, "tbl_flag", suspendWhenHidden = FALSE)


    #--------------------------------------------------------
    out_tbl <- eventReactive(input$tbl_go, {
      x <- try(collect(tbl(db.pool(), input$tbl_name)), silent = TRUE)
      validate(need(x, "Unable to fetch table"))

      x[, setdiff(names(x), "ts")]
    })

    output$tbl_x_uiOut <- renderUI({
      selectInput("tbl_x", tags$h5("X axis variable"),
                  choices = names(out_tbl()))
    })

    output$tbl_y_uiOut <- renderUI({
      selectInput("tbl_y", tags$h5("Y axis variable"),
                  choices = names(out_tbl()),
                  selected = names(out_tbl())[2])
    })

    out_plot <- reactive({
      x <- req(out_tbl())
      validate(
        need(!identical(input$tbl_x, input$tbl_y),
             "Please choose different variables for x and y")
      )

      ggplot(x, aes(!!as.name(input$tbl_x), !!as.name(input$tbl_y))) +
        geom_point() +
        ggtitle("Questionable plot")

    })

    out_text <- reactive({
      x <- req(out_tbl())
      # validate(
      #   need(!identical(input$tbl_x, input$tbl_y),
      #        "Please choose different variables for x and y")
      # )

      if (input$txt_display) {
        tagList(
          tags$strong("Header test"),
          tags$h5(paste("The table", input$tbl_name, "has been loaded"))
        )
      } else {
        NULL
      }
    })

    observe(mod_output_server("out_test", out_tbl, out_plot, out_text))
    # observe(mod_output_server("out_test", out_tbl, out_plot, NULL))
  }

  shiny::shinyApp(ui = ui, server = server, options = list(...))
}
