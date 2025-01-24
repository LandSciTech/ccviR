mod_B_ui <- function(id, title) {
  tabPanel(
    "Vulnerability Questions - B",
    fluidRow(
      column(
        12,
        h2("Vulnerability Questions"),
        p("This section scores factors associated with the species' indirect
                exposure to climate change (Section B), sensitivity and adaptive
                capacity (Section C), and modeled or documented responses to
                climate change (Section D). Questions from Sections C and D with
                a spatial component are adressed in the \"Spatial Vulnerability
                Questions\" tab and questions that only apply to certain taxa are
                only displayed if applicable. As a result, the question numbering
                is not sequential but will match the NatureServe version."),
        p("The NatureServe Guidelines for scoring each question can be accessed
                by clicking the info button next to the question. Use published studies,
                empirical data or expert opinion to support your responses. Provide
                detailed information about how the answer was reached in the comment boxes.
                To reflect uncertainty you may select more than one response to each question"),
        div(
          id = "secB",
          h3("Section B: Indirect Exposure to Climate Change"),
          h5("Evaluate for assessment area under consideration"),
          check_comment_ui("B1", "B1) Exposure to sea level rise:",
                           choiceNames = valueNms,
                           choiceValues = valueOpts),
          check_comment_ui("B2a", "B2 a) Distribution relative to natural barriers",
                           choiceNames = valueNms,
                           choiceValues = valueOpts),
          check_comment_ui("B2b", "B2 b) Distribution relative to anthropogenic barriers",
                           choiceNames = valueNms,
                           choiceValues = valueOpts),

          check_comment_ui("B3", "B3) Predicted impact of land use changes resulting from human responses to climate change",
                           choiceNames = valueNms[2:4],
                           choiceValues = valueOpts[2:4])
        ),
        br(),
      )
    )
  )
}

mod_B_server <- function(id) {

  moduleServer(id, function(input, output, session) {



  })

}
