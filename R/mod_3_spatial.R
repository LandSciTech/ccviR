mod_spatial_ui <- function(id, title) {
  tabPanel(
    "Spatial Data Analysis",
    fluidRow(
      column(
        12,
        div(
          id = "spatial",
          h2("Spatial Data Analysis"),
          p("The spatial data input in this section will be used to calculate
                  the exposure to climate change (Section A). It will also be used
                  to evaluate select questions in Section C that have a spatial
                  component. If provided, the range change raster(s) will be used to
                  evalaute questions about the modeled response to climate change
                  in Section D. Required datasets are indicated with", labelMandatory("a")),
          get_file_ui("clim_var_dir", "Folder location of prepared climate data",
                      type = "dir", mandatory = TRUE, spinner = TRUE),
          verbatimTextOutput("clim_var_error", ),
          br(),
          get_file_ui("range_poly_pth", "Range polygon shapefile", mandatory = TRUE),
          get_file_ui("assess_poly_pth", "Assessment area polygon shapefile", mandatory = TRUE),
          get_file_ui("ptn_poly_pth", "Physiological thermal niche file"),
          get_file_ui("nonbreed_poly_pth", "Non-breeding Range polygon shapefile"),
          selectInput("rng_chg_used", "Will a projected range change raster be supplied?",
                      c("No" = "no",
                        "Yes, one range change raster will be supplied for all scenarios" = "one",
                        "Yes, multiple range change rasters will be supplied, one for each scenario (Preferred)" = "multiple")),
          uiOutput("rng_chg_sel_ui"),
          conditionalPanel(condition = "input.rng_chg_used !== 'no'",
                           strong("Classification of projected range change raster"),
                           p("Enter the range of values in the raster corresponding to ",
                             "lost, maintained, gained and not suitable."),
                           from_to_ui("lost", "Lost:",  c(-1, -1)),
                           from_to_ui("maint", "Maintained:", c(0, 0)),
                           from_to_ui("gain", "Gained:", c(1,1)),
                           from_to_ui("ns", "Not Suitable:", c(99, 99)),
                           br(),
                           strong("Gain modifier"),
                           p("Range gains predicted based on future climate projections should be ",
                             "interpreted cautiously. It is important to consider whether ",
                             "the gains are likely to be realized. E.g. Is the species ",
                             "capable of dispersing to the new habitat and will factors other ",
                             "than climate be suitable in those areas?"),
                           p("To account for this you can set the gain modifier below to less",
                             " than 1 in order to down weight future range expansions when the",
                             " modelled response to climate change is being assessed. A value of 0 will ",
                             "ignore gains all together while 0.5 assumes that only half",
                             " the projected gains are realized and 1 assumes all gains are realized."),
                           numericInput("gain_mod", NULL, 1, min = 0, max = 1, step = 0.1),
                           textAreaInput("gain_mod_comm", "Gain modifier explanation")
          ),
          br(),
          h5("Click button to begin the spatial analysis or to re-run it",
             " after changing inputs:"),
          actionButton("startSpatial", "Run Spatial Analysis", class = "btn-primary"),
          br(),
          conditionalPanel(
            condition = "input.startSpatial > 0",
            shinycssloaders::withSpinner(verbatimTextOutput("spat_error"),
                                         proxy.height = "50px")
          ),
          br(),br(),
          actionButton("next2", "Next", class = "btn-primary"),
          br(), br()
        )
      )
    )
  )

}

mod_spatial_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    # Mandatory fields
    fieldsMandatory <- c("range_poly_pth", "assess_poly_pth")

  })

}
