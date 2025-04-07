
#' Test the B Questions module
#'
#' @noRd
#' @examples
#' mod_B_test()
#' mod_B_test(df_loaded = FALSE)

mod_B_test <- function(df_loaded = TRUE) {

  ui <- ui_setup(mod_B_ui(id = "test"))
  server <- function(input, output, session) {
    shinyOptions("file_dir" = "inst/extdata/")

    if(df_loaded) {
      df_loaded <- test_files()$saved$final %>%
        load_previous() %>%
        reactive()
    } else df_loaded <- reactive(NULL)

    mod_B_server(id = "test", df_loaded, parent_session = session)
  }

  shinyApp(ui, server)
}

mod_B_ui <- function(id) {

  ns <- NS(id)

  tabPanel(
    "Vulnerability Questions - B",
    fluidRow(
      column(
        12,
        h2("Section B: Indirect Exposure to Climate Change"),
        p("This section scores factors associated with the species' indirect",
          "exposure to climate change."),
        p("Questions that only apply to certain taxa are",
          "only displayed if applicable. As a result, the question numbering",
          "is not sequential but will match the NatureServe version."),
        p("The NatureServe Guidelines for scoring each question can be accessed",
          "by clicking the info button next to the question. Use published studies,",
          "empirical data or expert opinion to support your responses. Provide",
          "detailed information about how the answer was reached in the comment boxes.",
          "To reflect uncertainty you may select more than one response to each question"),
        div(
          id = ns("secB"),

          h3("Questions"),
          h5("Evaluate for assessment area under consideration"),
          check_comment_ui2(
            id, "B1", "B1) Exposure to sea level rise",
            choiceNames = valueNms,
            choiceValues = valueOpts),
          check_comment_ui2(
            id, "B2a", "B2 a) Distribution relative to natural barriers",
            choiceNames = valueNms,
            choiceValues = valueOpts),
          check_comment_ui2(
            id, "B2b", "B2 b) Distribution relative to anthropogenic barriers",
            choiceNames = valueNms,
            choiceValues = valueOpts),

          check_comment_ui2(
            id, "B3",
            "B3) Predicted impact of land use changes resulting from human responses to climate change",
            choiceNames = valueNms[2:4],
            choiceValues = valueOpts[2:4]),

          actionButton(ns("continue"), "Next", class = "btn-primary"),
          br(), br()
        )
      )
    )
  )
}

mod_B_server <- function(id, df_loaded, parent_session) {

  stopifnot(is.reactive(df_loaded))

  moduleServer(id, function(input, output, session) {

    # Setup --------------------
    # Continue Button
    observeEvent(input$continue, switch_tab("Vulnerability Questions - C", parent_session))
    observe(show_guidelines(input)) # Create Guideline buttons

    # Restore data ----------------
    observeEvent(df_loaded(), {
      update_restored2(df_loaded(), section = "vuln_qs", session)
    })

    # Return -------------------------------------------------
    list("b" = reactive(collect_questions(input, "B")))

  })

}
