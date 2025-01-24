mod_C_ui <- function(id, title) {
  tabPanel(
    "Vulnerability Questions - C",
    fluidRow(
      column(
        12,
        div(
          id = "secC",
          h3("Section C: Sensitivity and Adaptive Capacity"),
          check_comment_ui("C1", "C1) Dispersal and movements",
                           choiceNames = valueNms,
                           choiceValues = valueOpts),

          strong("C2 b) Predicted sensitivity to changes in precipitation, hydrology, or moisture regime:"),

          check_comment_ui("C2bii", "C2 b ii) physiological hydrological niche.",
                           choiceNames = valueNms,
                           choiceValues = valueOpts),

          check_comment_ui("C2c", "C2 c) Dependence on a specific disturbance regime likely to be impacted by climate change.",
                           choiceNames = valueNms[2:4],
                           choiceValues = valueOpts[2:4]),
          check_comment_ui("C2d", "C2 d) Dependence on ice, ice-edge, or snow-cover habitats.",
                           choiceNames = valueNms,
                           choiceValues = valueOpts),

          check_comment_ui("C3", "C3) Restriction to uncommon landscape/geological features or derivatives.",
                           choiceNames = valueNms[2:4],
                           choiceValues = valueOpts[2:4]),
          check_comment_ui("C4a", "C4 a) Dependence on other species to generate required habitat.",
                           choiceNames = valueNms[2:4],
                           choiceValues = valueOpts[2:4]),
          shinyjs::hidden(
            div(
              id = "animal_only",
              check_comment_ui("C4b", "C4 b) Dietary versatility (animals only).",
                               choiceNames = valueNms[2:4],
                               choiceValues = valueOpts[2:4])
            )
          ),
          shinyjs::hidden(
            div(
              id = "plant_only",
              check_comment_ui("C4c", "C4 c) Pollinator versatility (plants only).",
                               choiceNames = valueNms[2:4],
                               choiceValues = valueOpts[2:4])
            )
          ),
          check_comment_ui("C4d", "C4 d) Dependence on other species for propagule dispersal.",
                           choiceNames = valueNms[2:4],
                           choiceValues = valueOpts[2:4]),
          check_comment_ui("C4e", "C4 e) Sensitivity to pathogens or natural enemies.",
                           choiceNames = valueNms[2:4],
                           choiceValues = valueOpts[2:4]),
          check_comment_ui("C4f", "C4 f) Sensitivity to competition from native or non-native species.",
                           choiceNames = valueNms[2:4],
                           choiceValues = valueOpts[2:4]),
          check_comment_ui("C4g", "C4 g) Forms part of an interspecific interaction not covered by 4a-f.",
                           choiceNames = valueNms[2:4],
                           choiceValues = valueOpts[2:4]),

          check_comment_ui("C5a", "C5 a) Measured genetic variation.",
                           choiceNames = valueNms[2:4],
                           choiceValues = valueOpts[2:4]),
          conditionalPanel(
            "input.C5a == ''",
            check_comment_ui("C5b", "C5 b) Occurrence of bottlenecks in recent evolutionary history (use only if 5a is unknown).",
                             choiceNames = valueNms[2:4],
                             choiceValues = valueOpts[2:4]),

          ),
          conditionalPanel(
            "input.C5a == '' && input.C5b == ''",
            shinyjs::hidden(
              div(
                id = "plant_only2",
                check_comment_ui("C5c", "C5 c) Reproductive system (plants only; use only if C5a and C5b are unknown).",
                                 choiceNames = valueNms[2:4],
                                 choiceValues = valueOpts[2:4])
              )
            )
          ),

          check_comment_ui("C6", "C6) Phenological response to changing seasonal temperature and precipitation dynamics.",
                           choiceNames = valueNms[2:4],
                           choiceValues = valueOpts[2:4])
        ),
        br(),
      )
    )
  )



}

mod_C_server <- function(id) {

  moduleServer(id, function(input, output, session) {



  })

}
