
indexOutUI <- function(id) {
    uiOutput(NS(id, "index_result"), inline = TRUE)
}

indexOutServer <- function(id, ind_df) {
  moduleServer(id, function(input, output, session) {
    stopifnot(is.reactive(ind_df))

    output$index_result <- renderUI({
      tagList(
        fluidRow(column(12, htmlOutput(NS(id, "scenario_name")))),
        fluidRow(column(width = 6,
                        plotly::plotlyOutput(NS(id, "ind_gauge"),
                                             height = "190px",
                                             inline = F),
                        htmlOutput(NS(id, "ind_def"))),
                 column(width = 6,
                        plotly::plotlyOutput(NS(id, "mig_exp_gauge"),
                                             height = "190px",
                                             inline = F)),
        ),
        hr(style="border-color: grey;",
           .noWS = c("before", "after", "outside", "after-begin", "before-end"))

      )
    })

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
      paste(p(style = "text-align: left;", def, .noWS = "after"))
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
#'  @examples
#' ui <- fluidPage(
#'   indexOutUI("index_out"),
#'   p("some other text")
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
#'
#' # with multiple scenarios
#' ui <- fluidPage(
#'   h4("Calculate or re-calculate the index"),
#'   actionButton("calcIndex", "Calculate", class = "btn-primary")
#' )
#'
#' server <- function(input, output, session) {
#'
#'   ind_df <- reactive(data.frame(scenario_name = c("Scenario 1", "Scenario 2"),
#'                       index = c("HV", "LV"),
#'                       mig_exp = c("M", "H")))
#'
#'   observeEvent(input$calcIndex, {
#'     ind_ls <- ind_df() %>% arrange(desc(scenario_name)) %>% split(ind_df()$scenario_name)
#'
#'     purrr::map(ind_ls, ~insertUI(selector = paste0("#", "calcIndex"),
#'                                  where = "afterEnd",
#'                                  ui = indexOutUI(paste0("index_result",
#'                                                         .x$scenario_name))))
#'
#'     purrr::map(ind_ls, ~indexOutServer(paste0("index_result",
#'                                               .x$scenario_name),
#'                                        reactive(.x)))
#'   })
#' }
#'
#' shinyApp(ui, server)


