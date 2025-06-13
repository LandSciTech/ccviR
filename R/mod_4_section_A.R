
#' Test the Section A module
#'
#' @noRd
#' @examples
#' # Test with all spatial details
#' mod_A_test()
#'
#' # Test with minimum spatial required
#' mod_A_test(spatial = test_spatial(min_req = TRUE))
#'
#' # Test with non-migratory
#' mod_A_test(species = test_species() %>% mutate(mig = FALSE))
#'
#' # Test missing breaks (CCEI in this case)
#' sp <- test_data()
#' sp$clim_readme$brks_ccei <- ""
#' sp <- test_spatial(d = sp)
#' mod_A_test(spatial = sp)

mod_A_test <- function(spatial = test_spatial(), species = test_species()) {

  ui <- ui_setup(mod_A_ui(id = "test"))
  server <- function(input, output, session) {
    mod_A_server(id = "test", spatial, reactive(species), parent_session = session)
  }

  shinyApp(ui, server)
}

mod_A_ui <- function(id) {

  ns <- NS(id)

  tabPanel(
    "A - Exposure", value = "A",
    fluidRow(
      column(
        12,
        # # helpful for testing
        # shinyjs::runcodeUI(type = "textarea"),
        h2("Section A: Exposure to Local Climate Change"),
        p("This section displays the results of the spatial analysis that",
          "determines the species' exposure to climate change (Section A)."),
        p("The exposure maps are created by subtracting the future climate",
          "from the historical climate and classifying the results into six",
          "classes (low to high level of exposure) based on the median and",
          "1/2 the interquartile range. Thus, negative values for temperature",
          "indicate warmer conditions (\u00B0C) and negative values for moisture",
          "(mm) indicate drier conditions."),
        p("The tables below the maps outline the classes and the proportion",
          "of the species range in each class. The exposure multiplier is",
          "determined by the level of exposure. It is used to modify the",
          "sensitivity and adaptive capacity components of the index based",
          "on exposure to climate change."),

        h5("Note that these results are presented for review but cannot be altered."),

        h3("Temperature exposure"),
        uiOutput(ns("ui_texp")),

        h3("Moisture exposure"),
        uiOutput(ns("ui_cmd")),

        h3("Migratory exposure (Climate Change Exposure Index)"),
        uiOutput(ns("ui_ccei")),

        actionButton(ns("continue"), "Next", class = "btn-primary"),
        br(), br()
      )
    )
  )
}

mod_A_server <- function(id, spatial, species, parent_session) {

  purrr::map(spatial, ~stopifnot(is.reactive(.x)))

  # Split up reactives
  spat_res <- spatial$spat_res
  clim_vars <- spatial$clim_vars
  clim_readme <- spatial$clim_readme
  range_poly <- spatial$range_poly
  nonbreed_poly <- spatial$nonbreed_poly

  moduleServer(id, function(input, output, session) {

    # Setup ----------------------
    ns <- session$ns

    # Continue Button
    observeEvent(input$continue, switch_tab("B", parent_session))


    # Temperature Exposure ----------------------------

    output$ui_texp <- renderUI({
      spat_vuln_ui(range_poly(), clim_vars(),
                    id = id, ui_id = "texp",
                    desc = "\"Range Polygon\" and \"Prepared Climate Data\"")
    })

    output$map_texp <- leaflet::renderLeaflet({
      # Don't require checks because UI not present if no spatial
      make_map2(range_poly(), clim_vars()$mat, rast1_nm = "mat",
                rast1_lbl = c("1 High", "2", "3","4", "5", "6 Low"))
    })

    output$tbl_texp <- gt::render_gt({
      get_exposure_table(spat_res(), "MAT", clim_readme(), clim_readme()$brks_mat)
    })

    # Moisture Exposure ------------------------
    output$ui_cmd <- renderUI({
      spat_vuln_ui(range_poly(), clim_vars(),
                    id = id, ui_id = "cmd",
                    desc = "\"Range Polygon\" and \"Prepared Climate Data\"")
    })
    output$map_cmd <- leaflet::renderLeaflet({
      make_map2(range_poly(), clim_vars()$cmd, rast1_nm = "cmd",
                rast1_lbl = c("1 High", "2", "3","4", "5", "6 Low"))
    })

    output$tbl_cmd <- gt::render_gt({
      get_exposure_table(spat_res(), "CMD", clim_readme(), clim_readme()$brks_cmd)
    })


    # Migratory Exposure and CCEI ---------------------------------
    output$ui_ccei <- renderUI({
      validate(need(
        species()$mig, "No migratory exposure for non-migratory species"),
        errorClass = "alert")
      spat_vuln_ui(clim_vars()$ccei, nonbreed_poly(),
                    id = id, ui_id = "ccei",
                    desc = "\"CCEI\" and \"Non-Breeding Range\"",
                    optional = TRUE)
    })

    output$map_ccei <- leaflet::renderLeaflet({
      req(nonbreed_poly(), clim_vars()$ccei)
      make_map2(nonbreed_poly(), clim_vars()$ccei, rast1_nm = "ccei",
                rast1_lbl = c("1 Low", "2", "3", "4 High"))
    })

    output$tbl_ccei <- gt::render_gt({
      req(species()$mig)
      get_exposure_table(spat_res(), "CCEI", clim_readme(),
                         clim_readme()$brks_ccei)
    })

  })

}
