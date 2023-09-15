#' Shiny module for output plot and table for AMLR shiny apps
#'
#' Shiny module for output plot and table for AMLR shiny apps
#'
#' @name mod_output
#'
#' @param id module namespace, see \code{shiny::\link[shiny]{NS}}
#' @param ... UI elements to be displayed in the plot box,
#'   but below the plot and assocaited plot widgets
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
          column(12, uiOutput(ns("plot_uiOut"))),
          column(12, uiOutput(ns("plot_assoc_uiOut")))
        ),
        # box(
        #   width = 12,
        #   column(
        #     width = 11,
        #     uiOutput(ns("plot"))
        #     # conditionalPanel(
        #     #   condition = "input.plot_type == 'ggplot'", ns = ns,
        #     #   plotOutput(ns("plot_ggplot"))
        #     # ),
        #     # conditionalPanel(
        #     #   condition = "input.plot_type == 'plotly'", ns = ns,
        #     #   plotlyOutput(ns("plot_plotly"))
        #     # )
        #   ),
        #   column(
        #     width = 1,
        #     radioButtons(ns("plot_type"), tags$h5("Plot type"),
        #                  choices = c("ggplot", "plotly"),
        #                  selected = "ggplot")
        #     # conditionalPanel(
        #     #   condition = "input.plot_type == 'ggplot'", ns = ns,
        #     #   numericInput(ns("plot_height"), tags$h5("Plot height (pixels)"),
        #     #                value = 650, min = 0, step = 50, width = "200px"),
        #     #   numericInput(ns("plot_width"), tags$h5("Plot width (pixels)"),
        #     #                value = 900, min = 0, step = 50, width = "200px"),
        #     #   tags$br(),
        #     #   downloadButton(ns("plot_download"), "Save plot as PNG")
        #     # )
        #   )
        # ),
        ...
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
#' @param plot.res numeric; plot resolution.
#'   Value is passed to \code{\link[shiny]{renderPlot}},
#'   and used to determine output file dimensions.
#'
#' @returns Nothing
#'
#' @export
mod_output_server <- function(id, tbl.reac, plot.reac, plot.res = 96) {
  stopifnot(
    is.reactive(tbl.reac),
    is.reactive(plot.reac)
  )

  # NOTE for table and plot server sections:
  # - reactiveVal used to avoid printing duplicate validation messages
  # - renderUI/uiOutput used to avoid always having UI space used by
  #   plot/DT output widgets, eg when displaying validation messages



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

      #------------------------------------------------------------------------
      # Table

      ### Columns to display in table
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

      output$plot_ggplot <- renderPlot(plot_out(), res = plot.res)
      output$plot_plotly <- renderPlotly(ggplotly(req(plot_out())))
      # ggplotly(req(plot.reac()), height = plot_height(), width = plot_width())

      output$plot_uiOut <- renderUI({
        req(plot_out(), input$plot_type)

        if (input$plot_type == 'ggplot') {
          plotOutput(ns("plot_ggplot"))
        } else if (input$plot_type == 'plotly') {
          plotlyOutput(ns("plot_plotly"))
        } else {
          validate("bad plot option")
        }
      })

      ### Output plot associated widgets: ggplot/plotly and download button
      plot.istruthy <- reactiveVal(NULL)
      observe({
        plot.istruthy(NULL)
        plot.istruthy(isTruthy(plot_out()))
      })
      output$plot_assoc_uiOut <- renderUI({
        req(plot.istruthy())
        box(
          width = 12,
          fluidRow(
            column(3, radioButtons(ns("plot_type"), "Plot type to display",
                                   choices = c("ggplot", "plotly"),
                                   selected = "ggplot", inline = TRUE)),
            conditionalPanel(
              condition = "input.plot_type == 'ggplot'", ns = ns,
              column(
                width = 9, tags$br(),
                downloadButton(ns("plot_download"), "Save plot as PNG")
              )
            )
          )
        )
      })


      # plot_height <- reactive({
      #   validate(
      #     need(input$plot_height > 100, "The plot height must be at least 100")
      #   )
      #   input$plot_height
      # })
      #
      # plot_width <- reactive({
      #   validate(
      #     need(input$plot_width > 100, "The plot width must be at least 100")
      #   )
      #   input$plot_width
      # })
      #
      # # Output plot
      # output$plot_ggplot <- renderPlot({
      #   plot.reac()
      # }, height = plot_height, width = plot_width, units = "px", res = plot.res)


      # Download plot
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
    }
  )
}
