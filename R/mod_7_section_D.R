mod_D_ui <- function(id, title) {
  tabPanel(
    "Vulnerability Questions - D",
    fluidRow(
      column(
        12,
        div(
          id = "secD",
          h3("Section D: Documented or Modeled Response to Climate Change"),
          h5("(Optional - May apply across the range of a species)"),

          check_comment_ui("D1", "D1) Documented response to recent climate change. ",
                           choiceNames = valueNms,
                           choiceValues = valueOpts),

          check_comment_ui("D4", "D4) Occurrence of protected areas in modeled future distribution.",
                           choiceNames = valueNms[2:4],
                           choiceValues = valueOpts[2:4]),
          actionButton("next4", "Next", class = "btn-primary"),
          br(), br()
        )
      )
    )
  )
}

mod_D_server <- function(id) {

  moduleServer(id, function(input, output, session) {



  })

}
