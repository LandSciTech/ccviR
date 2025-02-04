
#' Test the spatial module
#'
#' @noRd
#' @examples
#' mod_A_test()

mod_A_test <- function(spatial_details) {

  ui <- ui_setup(mod_A_ui(id = "test"))
  server <- function(input, output, session) {
    mod_A_server(id = "test", spatial_details, parent_session = session)
  }

  shinyApp(ui, server)
}

mod_A_ui <- function(id) {

  ns <- NS(id)

  tabPanel(
    "Vulnerability Questions - A",
    fluidRow(
      column(
        12,
        # # helpful for testing
        # shinyjs::runcodeUI(type = "textarea"),
        h2("Section A: Exposure to Local Climate Change"),
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
        p("Note that these results are presented for review but cannot be altered."),
        div(
          id = ns("texp_map_div"),
          h3("Temperature exposure"),
          shinycssloaders::withSpinner(leaflet::leafletOutput(ns("texp_map"))),
          gt::gt_output(ns("texp_tbl"))
        ),
        br(),
        div(
          id = ns("cmd_map_div"),
          h3("Moisture exposure"),
          leaflet::leafletOutput(ns("cmd_map")),
          gt::gt_output(ns("cmd_tbl"))
        ),
        br(),
        div(
          h3("Migratory exposure (Climate Change Exposure Index)"),
          div(id = ns("missing_ccei"),
              HTML("<font color=\"#FF0000\"><b>Data set not provided.</b></font>
                         <br>CCEI data and a non-breeding range are needed to calculate
                         the migratory exposure."),
              br(),
              br()),
          div(
            id = ns("ccei_exp"),
            leaflet::leafletOutput(ns("ccei_map")),
            gt::gt_output(ns("tbl_ccei")))

        )
      )
    ),
    fluidRow(
      column(
        12,
        actionButton(ns("continue"), "Next", class = "btn-primary"),
        br(), br()
      )
    )
  )
}

mod_A_server <- function(id, spatial_details, parent_session) {

  purrr::map(spatial_details, ~stopifnot(is.reactive(.x)))

  # Split up reactives
  spat_res <- spatial_details$spat_res
  clim_vars <- spatial_details$clim_vars
  clim_readme <- spatial_details$clim_readme
  range_poly <- spatial_details$range_poly
  nonbreed_poly <- spatial_details$nonbreed_poly

  moduleServer(id, function(input, output, session) {

    # Setup ----------------------

    # Continue Button
    observeEvent(input$continue, switch_tab("Vulnerability Questions - B", parent_session))


    # Outputs ---------------------
    # Temperature Exposure
    output$texp_map <- leaflet::renderLeaflet({
      #req(!is.character(spat_res()))
      #req(doSpatial())
      req(clim_vars())
      req(range_poly())

      make_map(range_poly(), clim_vars()$mat, rast_nm = "mat",
               rast_lbl = c("1 High", "2", "3","4", "5", "6 Low"))
    })

    output$texp_tbl <- gt::render_gt({
      req(clim_readme())
      get_exposure_table(spat_res(), "MAT", clim_readme(), clim_readme()$brks_mat)
    })

    # Moisture Exposure
    output$cmd_map <- leaflet::renderLeaflet({
      #req(!is.character(spat_res()))
      #req(doSpatial())
      req(clim_vars())
      req(range_poly())

      make_map(range_poly(), clim_vars()$cmd, rast_nm = "cmd",
               rast_lbl = c("1 High", "2", "3","4", "5", "6 Low"))

    })

    output$cmd_tbl <- gt::render_gt({
      #req(spat_res2())
      req(clim_readme())
      get_exposure_table(spat_res(), "CMD", clim_readme(), clim_readme()$brks_cmd)
    })


    observe({
      #req(doSpatial())
      req(clim_vars())
      if(isTruthy(clim_vars()$ccei) && isTruthy(isolate(nonbreed_poly()))){
        shinyjs::hide("missing_ccei")
        shinyjs::show("ccei_exp")
      } else {
        shinyjs::hide("ccei_exp")
        shinyjs::show("missing_ccei")
      }
    })

    # Climate Change Exposure Index
    output$ccei_map <- leaflet::renderLeaflet({
      #req(!is.character(spat_res()))
      #req(doSpatial())
      req(clim_vars()$ccei)
      req(isolate(nonbreed_poly()))

      make_map(nonbreed_poly(), clim_vars()$ccei, rast_nm = "ccei",
               rast_lbl = c("1 Low", "2", "3", "4 High"))
    })

    output$tbl_ccei <- gt::render_gt({
      #req(spat_res2())
      if(is.null(clim_readme()$brks_ccei)){
        class_brks <- "4: (> 7);3: (6 - 7);2: (4 - 5);1: (< 4)"
      } else {
        class_brks <- clim_readme()$brks_ccei
      }
      get_exposure_table(spat_res(), "CCEI", clim_readme(), class_brks)
    })

  })

}
