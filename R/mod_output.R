#' Shiny module for output plot and table for AMLR shiny apps
#'
#' Shiny module for output plot and table for AMLR shiny apps
#'
#' @name mod_output
#'
#' @param id module namespace, see \code{shiny::\link[shiny]{NS}}
#' @param ... UI elements to be displayed in the plot box,
#'   below the plot and above the save plot button
#'
#' @export
mod_output_ui <- function(id, ...) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        status = "primary", width = 12, collapsible = TRUE,
        fluidRow(
          column(10, uiOutput(ns("plot_uiOut"))),
          column(
            width = 2,
            conditionalPanel(
              condition = "output.plot_assoc_flag", ns = ns,
              box(
                width = 12,
                tags$strong("Plot display"),
                radioButtons(ns("plot_type"), NULL,
                             choices = c("ggplot", "plotly"),
                             selected = "ggplot"),
                conditionalPanel(
                  condition = "input.plot_type == 'ggplot'", ns = ns,
                  numericInput(ns("plot_height"), tags$h5("height (pixels)"),
                               value = 400, min = 0, step = 50),
                  checkboxInput(ns("plot_width_auto"), "auto-width", value = TRUE),
                  conditionalPanel(
                    condition = "input.plot_width_auto != 1", ns = ns,
                    numericInput(ns("plot_width"), tags$h5("width (pixels)"),
                                 value = 750, min = 0, step = 50)
                  )
                )
              )
            )
          )
        ),
        ...,
        conditionalPanel(
          condition = "output.plot_assoc_flag", ns = ns,
          downloadButton(ns("plot_download"), "Save plot as PNG")
        )
      ),
      box(
        status = "primary", width = 12, collapsible = TRUE,
        uiOutput(ns("tbl_uiOut")),
        tags$br(),
        uiOutput(ns("tbl_assoc_uiOut"))
      )
    )
  )
}

#' @name mod_output
#'
#' @param tbl.reac reactive; data frame to be displayed in the table
#' @param plot.reac reactive; \code{\link[ggplot2]{ggplot}} object to be plotted
#'
#' @returns Nothing
#'
#' @export
mod_output_server <- function(id, tbl.reac, plot.reac) {
  stopifnot(
    is.reactive(tbl.reac),
    is.reactive(plot.reac)
  )

  # NOTE for table and plot server sections:
  # - reactiveVal used to avoid printing duplicate validation messages
  # - renderUI/uiOutput used to avoid always having UI space used by
  #   plot/DT output widgets, eg when displaying validation messages
  # - plot uses output object because none of widgets depend on plot values


  moduleServer(
    id,
    function(input, output, session) {
      # For server, the id is just the ns of this module, eg 'out'.
      #   Thus, one must use session$ns and not just NS.
      #   This is the rec in https://shiny.posit.co/r/articles/improve/modules/
      #   in the 'Using renderUI within modules' section.
      ns <- session$ns

      # Create parent namespace/id strings for output file name defaults
      ns.curr <- session$ns(NULL)
      parent.id <- if_else(
        grepl(ns.sep, ns.curr), strsplit(ns.curr, ns.sep)[[1]][-2], id
      )

      # Plot resolution
      # Passed to renderPlot, and used to determine output file dimensions.
      plot.res = 96


      #------------------------------------------------------------------------
      # Plot

      ### Output plot
      plot_out <- reactive({
        validate(
          need(inherits(plot.reac(), "ggplot") | is.null(plot.reac()),
               paste("mod_output_server's plot.reac must be a ggplot object;",
                     "please contact the database manager")),
        )
        plot.reac()
      })

      plot_height <- reactive({
        validate(
          need(input$plot_height > 100, "The plot height must be at least 100")
        )
        input$plot_height
      })
      plot_width <- reactive({
        if (input$plot_width_auto) {
          "auto"
        } else {
          validate(
            need(input$plot_width > 100, "The plot width must be at least 100")
          )
          input$plot_width
        }
      })

      output$plot_ggplot <- renderPlot({
        plot_out()
      }, height = plot_height, width = plot_width, units = "px", res = plot.res)
      output$plot_plotly <- renderPlotly(ggplotly(req(plot_out())))

      output$plot_uiOut <- renderUI({
        req(plot_out(), input$plot_type)

        if (input$plot_type == 'ggplot') {
          plotOutput(ns("plot_ggplot"), height = plot_height())
        } else if (input$plot_type == 'plotly') {
          plotlyOutput(ns("plot_plotly"))
        } else {
          validate("bad plot option")
        }
      })

      ### Output object for is plot valid, for conditionalPanel in UI
      output$plot_assoc_flag <- reactive(isTruthy(plot_out()))
      outputOptions(output, "plot_assoc_flag", suspendWhenHidden = FALSE)

      ### Download plot
      output$plot_download <- downloadHandler(
        filename = function() paste(parent.id, "plot.png", sep = "-"),
        content = function(file) {
          plot.id <- paste0("output_", session$ns("plot_ggplot"))
          x <- req(session$clientData[[paste0(plot.id, "_width")]]) / plot.res
          y <- req(session$clientData[[paste0(plot.id, "_height")]]) / plot.res

          # NOTE: if the user needs control over the resolution,
          #   must use png() device directly per
          #   https://github.com/tidyverse/ggplot2/issues/2276
          # ggsave docs have an example of this
          #   (https://ggplot2.tidyverse.org/reference/ggsave.html),
          #   basically just make sure to print the ggplot object

          ggsave(
            file, plot = plot_out(), device = "png",
            height = y, width = x, units = "in"
          )
        }
      )


      #------------------------------------------------------------------------
      # Table

      ### Table-associated widgets
      tbl.names <- reactiveVal(NULL)
      observe({
        tbl.names(NULL)
        tbl.names(names(req(tbl.reac())))
      })

      output$tbl_assoc_uiOut <- renderUI({
        req(tbl.names())

        box(
          width = 12,
          selectInput(
            session$ns("tbl_cols"), "Columns to display in table and output CSV",
            choices = as.list(tbl.names()), selected = tbl.names(),
            multiple = TRUE, selectize = TRUE
          ),
          downloadButton(ns("tbl_download"), "Download table as CSV"),
          actionButton(ns("tbl_cols_reset"), "Re-select all columns in original order")
        )
      })

      observeEvent(input$tbl_cols_reset, {
        updateSelectInput(session, "tbl_cols", selected = names(req(tbl.reac())))
      })

      ### Output table
      tbl_out <- reactive({
        validate(
          need(inherits(tbl.reac(), "data.frame"),
               paste("mod_output_server's tbl.reac must be a data frame;",
                     "please contact the database manager")),
          need(input$tbl_cols, "Please select at least one column to display")
        )
        req(all(input$tbl_cols %in% names(tbl.reac())))

        tbl.reac() %>% select(input$tbl_cols)
      })

      output$tbl <- renderDT(tbl_out(), options = list(scrollX = TRUE))
      output$tbl_uiOut <- renderUI({
        req(tbl_out())
        DTOutput(ns("tbl"))
      })

      ### Download table
      output$tbl_download <- downloadHandler(
        filename = function() paste(parent.id, "table.csv", sep = "-"),
        content = function(file) {
          write.csv(tbl_out(), file = file, row.names = FALSE, na = "")
        }
      )
    }
  )
}
