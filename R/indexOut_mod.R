
indexOutUI <- function(id) {
  uiOutput(NS(id, "index_result"), inline = TRUE)
}

indexOutServer <- function(id, ind_df) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$index_result <- renderUI({
      tagList(
        fluidRow(column(12, htmlOutput(ns("scenario_name")))),
        fluidRow(column(width = 6,
                        plotly::plotlyOutput(ns("ind_gauge"),
                                             height = "190px",
                                             inline = F),
                        htmlOutput(ns("ind_def"))),
                 column(width = 6,
                        plotly::plotlyOutput(ns("mig_exp_gauge"),
                                             height = "190px",
                                             inline = F)),
        ),
        hr(style="border-color: grey;",
           .noWS = c("before", "after", "outside", "after-begin", "before-end"))

      )
    })

    output$scenario_name <- renderText(paste(h4(style = "text-align: left;",
                                                ind_df$scenario_name)))

    output$ind_def <- renderText({
      index <- ind_df$index
      def <- case_when(index == "IE" ~ "Information entered about the species' vulnerability is inadequate to calculate an index score.",
                       index == "EV" ~ "Abundance and/or range extent within geographical area assessed extremely likely to substantially decrease or disappear by 2050.",
                       index == "HV" ~ "Abundance and/or range extent within geographical area assessed likely to decrease significantly by 2050.",
                       index == "MV" ~ "Abundance and/or range extent within geographical area assessed likely to decrease by 2050.",
                       index == "LV" ~ "Available evidence does not suggest that abundance and/or range extent within the geographical area assessed will change (increase/decrease) substantially by 2050. Actual range boundaries may change.",
                       TRUE ~ "")
      paste(p(style = "text-align: left;", def, .noWS = "after"))
    })

    output$ind_gauge <- plotly::renderPlotly({
      plot_index_gauge(ind_df$index, type = "index")
    })

    output$mig_exp_gauge <- plotly::renderPlotly({
      plot_index_gauge(ind_df$mig_exp, type = "mig_exp")
    })

  })
}
