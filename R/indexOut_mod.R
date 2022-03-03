
indexOutUI <- function(id) {
  tagList(
    fluidRow(htmlOutput(NS(id, "scenario_name"))),
    fluidRow(column(width = 6,
                    align = "center",
                    h4("Climate Change Vulnerability Index"),
                    plotly::plotlyOutput(NS(id, "ind_gauge"), height = "150px",
                                         inline = TRUE),
                    htmlOutput(NS(id, "ind_def"))),
             column(width = 6,
                    align = "center",
                    h4("Migratory Exposure Index"),
                    plotly::plotlyOutput(NS(id, "mig_exp_gauge"))))
  )
}

indexOutServer <- function(id, ind_df) {
  moduleServer(id, function(input, output, session) {
    stopifnot(is.reactive(ind_df))
    output$scenario_name <- renderText(paste(h4(style = "text-align: left;",
                                          ind_df()$scenario_name)))

    output$ind_def <- renderText({
      index <- ind_df()$index
      def <- case_when(index == "IE" ~ "Information entered about the species' vulnerability is inadequate to calculate an index score.",
                index == "EV" ~ "Abundance and/or range extent within geographical area assessed extremely likely to substantially decrease or disappear by 2050.",
                index == "HV" ~ "Abundance and/or range extent within geographical area assessed likely to decrease significantly by 2050.",
                index == "MV" ~ "Abundance and/or range extent within geographical area assessed likely to decrease by 2050.",
                index == "LV" ~ "Available evidence does not suggest that abundance and/or range extent within the geographical area assessed will change (increase/decrease) substantially by 2050. Actual range boundaries may change.",
                TRUE ~ "")
      paste(p(style = "text-align: left;", def))
    })

    output$ind_gauge <- plotly::renderPlotly({
      plt_index_gauge(ind_df()$index, type = "index")
    })

    output$mig_exp_gauge <- plotly::renderPlotly({
      plt_index_gauge(ind_df()$mig_exp, type = "mig_exp")
    })

  })
}

#'
#'
#' @examples
#' ui <- fluidPage(
#'   indexOutUI("index_out")
#' )
#'
#' server <- function(input, output, session) {
#'
#'   indexOutServer("index_out", reactive(data.frame(scenario_name = "Scenario name",
#'                                          index = "HV",
#'                                          mig_exp = "M")))
#' }
#'
#' shinyApp(ui, server)




