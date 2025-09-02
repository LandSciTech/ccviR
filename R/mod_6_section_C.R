#' Test the C Questions module
#'
#' @noRd
#' @examples
#' mod_C_test()
#' mod_C_test(tax_grp = "Reptile") # Test different questions
#' mod_C_test(df_loaded = test_df_loaded())
#' mod_C_test(input_files = test_files(min_req = TRUE)) # Min-required only

mod_C_test <- function(df_loaded = NULL, input_files = test_files(),
                       tax_grp = "Vascular Plant") {


  ui <- ui_setup(mod_C_ui(id = "test"))
  server <- function(input, output, session) {
    shinyOptions("file_dir" = "inst/extdata/")

    # Speed up data by only loading required for Section C
    i <- c("assess_poly_pth", "rng_poly_pth", "clim_dir", "ptn_poly_pth", "scn_nms")
    spatial <- test_data(f = input_files[i]) %>%
      test_spatial(d_paths = input_files[i], quiet = TRUE)

    mod_C_server(id = "test", reactive(df_loaded), spatial,
                 tax_grp = reactive(tax_grp), parent_session = session)
  }

  shinyApp(ui, server)
}

mod_C_ui <- function(id) {

  ns <- NS(id)

  tabPanel(
    "C - Sensitivity and Adaptive Capacity", value = "C",
    fluidRow(
      column(
        12,
        h2("Section C: Sensitivity and Adaptive Capacity"),
        p("This section scores factors associated with the species' sensitivity and adaptive
                capacity."),
        p("Questions with a spatial component are also included. ",
          "The spatial data analysis evaluates these factors and pre-selects a response accordingly. ",
          "The data used to pre-select a response is
                shown in a map and table accompanying each question. The pre-selected
                response can be changed if needed and multiple responses can be
                selected to reflect uncertainty."),
        p("Questions that only apply to certain taxa are
                only displayed if applicable. As a result, the question numbering
                is not sequential but will match the NatureServe version."),
        p("The NatureServe Guidelines for scoring each question can be accessed
                by clicking the info button next to the question. Use published studies,
                empirical data or expert opinion to support your responses. Provide
                detailed information about how the answer was reached in the comment boxes.
                To reflect uncertainty you may select more than one response to each question"),
        div(
          id = "secC",
          h3("Questions"),
          check_comment_ui(id, "C1", "C1) Dispersal and movements",
                           choiceNames = valueNms,
                           choiceValues = valueOpts),

          q5("C2 a) Predicted sensitivity to temperature and moisture changes"),
          div(style = "margin-left:1em;",
              q5("C2 ai) Historical thermal niche"),
              uiOutput(ns("ui_C2ai")),

              q5("C2 aii) Physiological thermal niche"),
              uiOutput(ns("ui_C2aii"))
          ),

          q5("C2 b) Predicted sensitivity to changes in precipitation, hydrology, or moisture regime"),
          div(style = "margin-left:1em;",
              q5("C2 bi) Historical hydrological niche"),
              uiOutput(ns("ui_C2bi")),

              check_comment_ui(id, "C2bii", "C2 bii) Physiological hydrological niche",
                                choiceNames = valueNms,
                                choiceValues = valueOpts)
          ),

          check_comment_ui(
            id, "C2c",
            "C2 c) Dependence on a specific disturbance regime likely to be impacted by climate change",
            choiceNames = valueNms[2:4],
            choiceValues = valueOpts[2:4]),

          check_comment_ui(
            id, "C2d",
            "C2 d) Dependence on ice, ice-edge, or snow-cover habitats",
            choiceNames = valueNms,
            choiceValues = valueOpts),

          check_comment_ui(
            id, "C3",
            "C3) Restriction to uncommon landscape/geological features or derivatives",
            choiceNames = valueNms[2:4],
            choiceValues = valueOpts[2:4]),

          check_comment_ui(
            id, "C4a",
            "C4 a) Dependence on other species to generate required habitat",
            choiceNames = valueNms[2:4],
            choiceValues = valueOpts[2:4]),

          shinyjs::hidden(
            div(
              id = ns("animal_only"),
              check_comment_ui(
                id, "C4b",
                "C4 b) Dietary versatility (animals only)",
                choiceNames = valueNms[2:4],
                choiceValues = valueOpts[2:4])
            )
          ),

          shinyjs::hidden(
            div(
              id = ns("plant_only"),
              check_comment_ui(
                id, "C4c",
                "C4 c) Pollinator versatility (plants only)",
                choiceNames = valueNms[2:4],
                choiceValues = valueOpts[2:4])
            )
          ),

          check_comment_ui(
            id, "C4d",
            "C4 d) Dependence on other species for propagule dispersal",
            choiceNames = valueNms[2:4],
            choiceValues = valueOpts[2:4]),

          check_comment_ui(
            id, "C4e",
            "C4 e) Sensitivity to pathogens or natural enemies",
            choiceNames = valueNms[2:4],
            choiceValues = valueOpts[2:4]),

          check_comment_ui(
            id, "C4f",
            "C4 f) Sensitivity to competition from native or non-native species",
            choiceNames = valueNms[2:4],
            choiceValues = valueOpts[2:4]),

          check_comment_ui(
            id, "C4g",
            "C4 g) Forms part of an interspecific interaction not covered by 4a-f",
            choiceNames = valueNms[2:4],
            choiceValues = valueOpts[2:4]),

          check_comment_ui(id, "C5a", "C5 a) Measured genetic variation",
                           choiceNames = valueNms[2:4],
                           choiceValues = valueOpts[2:4]),

          conditionalPanel(
            "input.C5a == ''",
            check_comment_ui(
              id, "C5b",
              "C5 b) Occurrence of bottlenecks in recent evolutionary history (use only if 5a is unknown)",
              choiceNames = valueNms[2:4],
              choiceValues = valueOpts[2:4]),
            ns = NS(id)
          ),

          conditionalPanel(
            "input.C5a == '' && input.C5b == ''",
            shinyjs::hidden(
              div(
                id = ns("plant_only2"),
                check_comment_ui(
                  id, "C5c",
                  "C5 c) Reproductive system (plants only; use only if C5a and C5b are unknown)",
                  choiceNames = valueNms[2:4],
                  choiceValues = valueOpts[2:4])
              )
            ),
            ns = NS(id)
          ),

          check_comment_ui(
            id, "C6",
            "C6) Phenological response to changing seasonal temperature and precipitation dynamics",
            choiceNames = valueNms[2:4],
            choiceValues = valueOpts[2:4]),

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

mod_C_server <- function(id, df_loaded, spatial, tax_grp, parent_session) {

  purrr::map(spatial, ~stopifnot(is.reactive(.x)))

  # Split up reactives
  spat_res <- spatial$spat_res
  clim_vars <- reactive({spatial$mapping_layers()$clim_vars})
  clim_readme <- spatial$clim_readme
  range_poly <- reactive({spatial$mapping_layers()$range_poly_assess})
  range_poly_clim <- reactive({spatial$mapping_layers()$range_poly_clim})
  ptn_poly <- reactive({spatial$mapping_layers()$ptn_poly})
  nonbreed_poly <- reactive({spatial$mapping_layers()$nonbreed_poly})

  moduleServer(id, function(input, output, session) {

    # Setup --------------------
    ns <- session$ns

    # Continue Button
    observeEvent(input$continue, switch_tab("D", parent_session))
    observe(show_guidelines(input)) # Create Guideline buttons

    # Show correct questions by taxonomic groups
    observe(show_questions(tax_grp()))

    # Restore data ----------------
    observeEvent(df_loaded(), {
      update_restored(df_loaded(), section = "vuln_qs_C", session)
    })



    # Spatial Questions ---------------------

    ## C2 ai ------------
    output$ui_C2ai <- renderUI({
      spat_vuln_ui(range_poly(), clim_vars(),
                    id = id, ui_id = "C2ai",
                    desc = "\"Range Polygon\" and \"Prepared Climate Data\"",
                    spat_df = spat_res(), input = input, q = TRUE)
    })
    outputOptions(output, "ui_C2ai", suspendWhenHidden = FALSE) # After creation
    observe(spat_vuln_hide("C2ai", range_poly(), clim_vars()))

    output$map_C2ai <- leaflet::renderLeaflet({
      make_map(range_poly_clim(), rast1 = clim_vars()$htn, rast1_nm = "htn",
                rast1_lbl = c("1 Low", "2", "3", "4 High"))
    })

    output$tbl_C2ai <- gt::render_gt({
      exp_tbl <- spat_res() %>%
        select(matches("HTN_\\d")) %>%
        rename_at(vars(contains("HTN")),
                  ~stringr::str_replace(.x, "HTN_", "Class ")) %>%
        tidyr::pivot_longer(
          cols = contains("Class"),
          names_to = "Sensitivity Class", values_to = "Proportion of Range") %>%
        transmute(
          `Sensitivity Class` =
            stringr::str_replace(.data$`Sensitivity Class`, "Class 1", "1 - Low") %>%
            stringr::str_replace("Class 4", "4 - High") %>%
            stringr::str_remove("Class"), .data$`Proportion of Range`) %>%
        distinct() %>%
        mutate(`Historical Temperature Variation` =
                 c("> 43.0", "26.3 - 31.8", "20.8 - 26.3" ,"< 20.8")) %>%
        mutate_if(is.numeric, round, digits = 2) %>%
        select("Sensitivity Class", "Historical Temperature Variation",
               "Proportion of Range") %>%
        gt::gt() %>%
        gt::cols_label(
          `Historical Temperature Variation` =
            gt::html("Historical Temperature Variation (&deg;C)")) %>%
        gt::tab_options(table.width = 600,
                        table.font.size = 14,
                        column_labels.padding.horizontal = 10,
                        column_labels.padding = 2,
                        data_row.padding = 2) %>%
        gt::cols_align(align = "center", columns = c(2, 3)) %>%
        gt::tab_style(style = gt::cell_text(weight = "bold", v_align = "middle"),
                      location = gt::cells_column_labels(columns = everything()))
    })


    ## C2 aii ----
    output$ui_C2aii <- renderUI({
      spat_vuln_ui(range_poly(), ptn_poly(),
                    id = id, ui_id = "C2aii",
                    desc = "\"Physiological Thermal Niche\"",
                    spat_df = spat_res(), input = input, q = TRUE, optional = TRUE)
    })
    outputOptions(output, "ui_C2aii", suspendWhenHidden = FALSE) # After creation

    output$map_C2aii <- leaflet::renderLeaflet({
      make_map(poly1 = range_poly(), poly2 = ptn_poly(), poly2_nm = "ptn")
    })

    output$tbl_C2aii <- gt::render_gt({
      exp_df <-  spat_res() %>%
        select(contains("PTN", ignore.case = FALSE)) %>%
        tidyr::pivot_longer(cols = contains("PTN", ignore.case = FALSE),
                            names_to = "Variable", values_to = "Proportion of Range") %>%
        mutate_if(is.numeric, round, digits = 2) %>%
        distinct() %>%
        gt::gt() %>%
        gt::tab_options(table.font.size = 14,
                        column_labels.padding.horizontal = 10,
                        column_labels.padding = 2,
                        data_row.padding = 2) %>%
        gt::cols_align(align = "center", columns = everything()) %>%
        gt::tab_style(style = gt::cell_text(weight = "bold", v_align = "middle"),
                      location = gt::cells_column_labels(columns = everything()))
    })

    ## C2 bi ---------
    output$ui_C2bi <- renderUI({
      spat_vuln_ui(range_poly(), clim_vars(),
                    id = id, ui_id = "C2bi",
                    desc = "\"Range Polygon\" and \"Prepared Climate Data\"",
                    spat_df = spat_res(), input = input, q = TRUE)
    })
    outputOptions(output, "ui_C2bi", suspendWhenHidden = FALSE)
    observe(spat_vuln_hide("C2bi", range_poly(), clim_vars()))

    output$map_C2bi <- leaflet::renderLeaflet({
      make_map(poly1 = range_poly_clim(), rast1 = clim_vars()$map,
                rast1_nm = "map")
    })

    output$tbl_C2bi <- gt::render_gt({
      exp_df <-  spat_res() %>%
        select("MAP_max", "MAP_min") %>%
        rename(`Min MAP` = .data$MAP_min, `Max MAP` = .data$MAP_max) %>%
        distinct() %>%
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
    questions <- reactive(collect_questions(input, "C", tax_grp()))

    # Data Completeness -----------
    output$completeness <- renderUI({ # Use UI when rendering HTML
      report_n(questions())
    })

    # Return -------------------------------------------------
    list("c" = questions)

  })

}
