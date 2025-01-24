mod_A_ui <- function(id, title) {
  tabPanel(
    "Exposure Results",
    fluidRow(
      column(
        12,
        # # helpful for testing
        # shinyjs::runcodeUI(type = "textarea"),
        h2("Exposure Results"),
        p("This section displays the results of the spatial analysis that
                determines the species' exposure to climate change (Section A)."),
        p("The exposure maps are created by subtracting the future climate
                from the historical climate and classifying the results into six
                classes (low to high level of exposure) based on the median and
                1/2 the interquartile range. Thus, negative values for temperature
                indicate warmer conditions (\u00B0C) and negative values for moisture
                (mm) indicate drier conditions."),
        p("The tables below the maps outline the classes and the proportion
                of the species range in each class. The exposure multiplier is
                determined by the level of exposure. It is used to modify the
                sensitivity and adaptive capacity components of the index based
                on exposure to climate change."),
        div(
          id = "texp_map_div",
          h3("Temperature exposure"),
          shinycssloaders::withSpinner(leaflet::leafletOutput("texp_map")),
          gt::gt_output("texp_tbl")
        ),
        br(),
        div(
          id = "cmd_map_div",
          h3("Moisture exposure"),
          leaflet::leafletOutput("cmd_map"),
          gt::gt_output("cmd_tbl")
        ),
        br(),
        div(
          h3("Migratory exposure (Climate Change Exposure Index)"),
          div(id = "missing_ccei",
              HTML("<font color=\"#FF0000\"><b>Data set not provided.</b></font>
                         <br>CCEI data and a non-breeding range are needed to calculate
                         the migratory exposure."),
              br(),
              br()),
          div(
            id = "ccei_exp",
            leaflet::leafletOutput("ccei_map"),
            gt::gt_output("tbl_ccei"))

        )
      )
    ),
    fluidRow(
      column(
        12,
        actionButton("next3", "Next", class = "btn-primary"),
        br(), br()
      )
    )
  )
}

mod_A_server <- function(id) {

  moduleServer(id, function(input, output, session) {



  })

}
