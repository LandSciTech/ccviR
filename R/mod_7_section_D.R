#' Test the D Questions module
#'
#' @noRd
#' @examples
#' mod_D_test()
#' mod_D_test(df_loaded = test_df_loaded())
#' mod_D_test(df_loaded = test_df_loaded("questions_only"),
#'            input_files = test_files(min_req = TRUE))
#' mod_D_test(input_files = test_files(min_req = TRUE))

mod_D_test <- function(df_loaded = NULL, input_files = test_files()) {

  ui <- ui_setup(mod_D_ui(id = "test"))
  server <- function(input, output, session) {
    shinyOptions("file_dir" = "inst/extdata/")

    spatial <- test_data(f = input_files) %>%
      test_spatial(d_paths = input_files)

    mod_D_server(id = "test", reactive(df_loaded),
                 spatial, parent_session = session)
  }

  shinyApp(ui, server)
}

mod_D_ui <- function(id) {

  ns <- NS(id)

  tabPanel(
    "D - Documented or Modeled Response", value = "D",
    fluidRow(
      column(
        12,
        h2("Section D: Documented or Modeled Response to Climate Change"),
        p("This section scores factors associated with the species' modeled or",
          "documented responses to climate change"),
        p("Questions with a spatial component are also included. ",
          "The spatial data analysis evaluates these factors and pre-selects a ",
          "response accordingly. ",
          "The data used to pre-select a response is shown in a map and table ",
          "accompanying each question. The pre-selected response can be changed",
          "if needed and multiple responses can be selected to reflect uncertainty."),
        p("Questions that only apply to certain taxa are only displayed if ",
          "applicable. As a result, the question numbering is not sequential ",
          "but will match the NatureServe version."),
        p("The NatureServe Guidelines for scoring each question can be accessed",
          "by clicking the info button next to the question. Use published studies,",
          "empirical data or expert opinion to support your responses. Provide",
          "detailed information about how the answer was reached in the comment boxes.",
          "To reflect uncertainty you may select more than one response to each question"),

        div(
          id = ns("secD"),

          h3("Questions"),
          h5("(Optional - May apply across the range of a species)"),

          check_comment_ui2(id, "D1", "D1) Documented response to recent climate change",
                           choiceNames = valueNms,
                           choiceValues = valueOpts),

          q5(span("Modeled future range change (for D2 and D3)", class = "bigger")),
          uiOutput(ns("ui_D2D3")),

          q5("D2) Modeled future (2050) change in population or range size"),
          uiOutput(ns("ui_D2")),

          q5("D3) Overlap of modeled future (2050) range with current range"),
          uiOutput(ns("ui_D3")),

          q5("D4) Occurrence of protected areas in modeled future distribution"),
          uiOutput(ns("ui_D4")),

          div(style = "display:inline-block",
              actionButton(ns("continue"), "Next", class = "btn-primary"),
              uiOutput(ns("completeness"), class = "button-status")
          ),
          br(), br()
        )
      )
    )
  )
}

mod_D_server <- function(id, df_loaded, spatial, parent_session) {

  purrr::map(spatial, ~stopifnot(is.reactive(.x)))

  # Split up reactives
  spat_res <- spatial$spat_res
  hs_rast <- spatial$hs_rast
  clim_readme <- spatial$clim_readme
  range_poly <- spatial$range_poly
  assess_poly <- spatial$assess_poly
  protected_poly <- spatial$protected_poly
  hs_rcl_mat <- spatial$hs_rcl_mat

  moduleServer(id, function(input, output, session) {

    # Setup --------------------

    # Continue Button
    observeEvent(input$continue, switch_tab("results", parent_session))
    observe(show_guidelines(input)) # Create Guideline buttons

    # Restore data ----------------
    observeEvent(df_loaded(), {

      update_restored2(df_loaded(), section = "vuln_qs_D", session)
    })

    # reclassify raster
    hs_rast2 <- reactive({
      req(hs_rast())
      rast <- terra::classify(hs_rast(), rcl = hs_rcl_mat(), right = NA)
    })

    # Spatial data ---------------------

    ## D2 and D3 ------
    output$ui_D2D3 <- renderUI({
      spat_vuln_ui2(
        range_poly(), hs_rast2(), assess_poly(),
        id = id, ui_id = "D2D3",
        desc = "\"Projected Range Changes\"",
        spat_df = spat_res(), input = input,
        optional = TRUE)
    })
    outputOptions(output, "ui_D2D3", suspendWhenHidden = FALSE) # After creation


    output$map_D2D3 <- leaflet::renderLeaflet({
      make_map2(
        poly1 = range_poly(), rast1 = hs_rast2(),
        poly2 = assess_poly(), poly2_nm = "assess_poly",
        rast1_nm = "hs_rast",
        rast1_lbl = data.frame(label = c("Not suitable", "Lost", "Maintained", "Gained"),
                               value = c(0, 1, 2, 3)))
    })

    output$tbl_D2D3 <- gt::render_gt({
      exp_df <-  spat_res() %>%
        select(`Scenario Name` = .data$scenario_name,
               `% Range Lost` = .data$range_change,
               `% Maintained` = .data$range_overlap) %>%
        mutate_if(is.numeric, round, digits = 2) %>%
        gt::gt() %>%
        gt::tab_options(table.font.size = 14,
                        column_labels.padding.horizontal = 10,
                        column_labels.padding = 2,
                        data_row.padding = 2) %>%
        gt::cols_align(align = "center", columns = everything()) %>%
        gt::tab_style(style = gt::cell_text(weight = "bold", v_align = "middle"),
                      location = gt::cells_column_labels(columns = everything()))
    })

    ## D2 ------
    output$ui_D2 <- renderUI({
      spat_vuln_ui2(
        range_poly(), hs_rast2(), assess_poly(),
        id = id, ui_id = "D2",
        spat_df = spat_res(), input = input, q = TRUE, map_table = FALSE,
        multi_stop = TRUE, optional = TRUE)
    })

    outputOptions(output, "ui_D2", suspendWhenHidden = FALSE) # After creation

    ## D3 ------
    output$ui_D3 <- renderUI({
      spat_vuln_ui2(
        range_poly(), hs_rast2(), assess_poly(),
        id = id, ui_id = "D3",
        spat_df = spat_res(), input = input, q = TRUE, map_table = FALSE,
        multi_stop = TRUE, optional = TRUE)
    })
    outputOptions(output, "ui_D3", suspendWhenHidden = FALSE) # After creation

    ## D4 --------------------------

    # UI Inputs
    output$ui_D4 <- renderUI({
      spat_vuln_ui2(
        range_poly(), hs_rast2(), protected_poly(), assess_poly(),
        id = id, ui_id = "D4",
        desc = "\"Projected Range Changes\" and \"Protected Areas\"",
        spat_df = spat_res(), input = input, q = TRUE, multi_stop = TRUE,
        optional = TRUE)
    })
    # This makes sure that the value is updated even if the tab isn't reopened
    outputOptions(output, "ui_D4", suspendWhenHidden = FALSE)

    output$map_D4 <- leaflet::renderLeaflet({
      map_protected(assess_poly(), protected_poly(), hs_rast2())
    })

    output$tbl_D4 <- gt::render_gt({
      exp_df <-  spat_res() %>%
        select(`Scenario Name` = .data$scenario_name,
               `% Protected` = .data$protected) %>%
        mutate_if(is.numeric, round, digits = 2) %>%
        gt::gt() %>%
        gt::tab_options(table.font.size = 14,
                        column_labels.padding.horizontal = 10,
                        column_labels.padding = 2,
                        data_row.padding = 2) %>%
        gt::cols_align(align = "center", columns = everything()) %>%
        gt::tab_style(style = gt::cell_text(weight = "bold", v_align = "middle"),
                      location = gt::cells_column_labels(columns = everything()))
    })

    # Questions --------------
    questions <- reactive({
      collect_questions(input, "D", spatial = spat_res())})

    # Data Completeness -----------
    output$completeness <- renderUI({ # Use UI when rendering HTML
      report_n(questions())
    })

    # Return -------------------------------------------------
    list("d" = questions)

  })

}
