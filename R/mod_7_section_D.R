mod_D_ui <- function(id) {

  ns <- NS(id)

  tabPanel(
    "Vulnerability Questions - D",
    fluidRow(
      column(
        12,
        h2("Section D: Documented or Modeled Response to Climate Change"),
        p("This section scores factors associated with the species' modeled or documented responses to
                climate change"),
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
          id = ns("secD"),
          h3("Questions"),
          h5("(Optional - May apply across the range of a species)"),

          check_comment_ui2(id, "D1", "D1) Documented response to recent climate change. ",
                           choiceNames = valueNms,
                           choiceValues = valueOpts),

          h4("Modeled future range change"),
          spat_vuln_ui2(id, "D2_3", chk_box = FALSE),
          fluidRow(column(9, strong("D2) Modeled future (2050) change in population or range size")),
                   column(1, actionButton(ns(paste0("help_", "D2")), label = "", icon = icon("info")))),
          uiOutput(ns("box_D2")),
          br(),
          fluidRow(column(9, strong("D3) Overlap of modeled future (2050) range with current range"),),
                   column(1, actionButton(ns(paste0("help_", "D3")), label = "", icon = icon("info")))),
          uiOutput(ns("box_D3")),

          check_comment_ui2(id, "D4", "D4) Occurrence of protected areas in modeled future distribution.",
                           choiceNames = valueNms[2:4],
                           choiceValues = valueOpts[2:4]),
          actionButton(ns("continue"), "Next", class = "btn-primary"),
          br(), br()
        )
      )
    )
  )
}

mod_D_server <- function(id, df_loaded, spatial_details, parent_session) {

  purrr::map(spatial_details, ~stopifnot(is.reactive(.x)))

  # Split up reactives
  spat_res <- spatial_details$spat_res
  hs_rast <- spatial_details$hs_rast
  clim_readme <- spatial_details$clim_readme
  range_poly <- spatial_details$range_poly
  assess_poly <- spatial_details$assess_poly
  hs_rcl_mat <- spatial_details$hs_rcl_mat

  moduleServer(id, function(input, output, session) {

    # Setup --------------------

    # Continue Button
    observeEvent(input$continue, switch_tab("Index Results", parent_session))
    observe(show_guidelines(input)) # Create Guideline buttons

    # Restore data ----------------
    observeEvent(df_loaded(), {
      update_restored2(df_loaded(), section = "vuln_qs", session)
    })

    # Spatial data ---------------------

    # D2 and D3 ------
    observe({
      spat_vuln_hide2("D2_3", spatial = hs_rast(), values = spat_res()["range_change"])
    })

    # reclassify raster
    hs_rast2 <- reactive({
      req(hs_rast())
      rast <- terra::classify(hs_rast(), rcl = hs_rcl_mat(), right = NA)
    })

    output$map_D2_3 <- leaflet::renderLeaflet({
      req(hs_rast2())
      make_map(
        poly1 = range_poly(), rast = hs_rast2(),
        poly2 = assess_poly(), poly2_nm = "assess_poly",
        rast_nm = "hs_rast",
        rast_lbl = data.frame(label = c("Not suitable", "Lost", "Maintained", "Gained"),
                              value = c(0, 1, 2, 3)))
    })

    output$tbl_D2_3 <- gt::render_gt({
      req(spat_res())
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

    output$box_D2 <- renderUI({
      req(spat_res())
      # get previous comment
      prevCom <- isolate(input$comD2)
      prevCom <- ifelse(is.null(prevCom), "", prevCom)
      box_val <- pull(spat_res(), .data$D2)

      if(nrow(spat_res()) > 1 & isTruthy(spat_res()$range_change)) {
        valueNm <- valueNms[4 - box_val]
        div(strong("Calculated effect on vulnerability:"),
            HTML("<font color=\"#FF0000\"><b> Spatial results can not be edited when multiple scenarios are provided.</b></font>"),
            HTML(paste0("<p>", clim_readme()$Scenario_Name, ": ", valueNm, "</p>")))

      } else {
        check_comment_ui2(id, "D2", HTML("Calculated effect on vulnerability:"),
                         choiceNames = valueNms,
                         choiceValues = valueOpts,
                         selected = box_val,
                         com = prevCom,
                         guide = FALSE)
      }

    })

    # This makes sure that the value is updated even if the tab isn't reopened
    outputOptions(output, "box_D2", suspendWhenHidden = FALSE)

    output$box_D3 <- renderUI({
      req(spat_res())
      # get previous comment
      prevCom <- isolate(input$comD3)
      prevCom <- ifelse(is.null(prevCom), "", prevCom)
      box_val <- pull(spat_res(), .data$D3)

      if(nrow(spat_res()) > 1 & isTruthy(spat_res()$range_overlap)){
        valueNm <- valueNms[4 - box_val]
        div(strong("Calculated effect on vulnerability:"),
            HTML("<font color=\"#FF0000\"><b> Spatial results can not be edited when multiple scenarios are provided.</b></font>"),
            HTML(paste0("<p>", clim_readme()$Scenario_Name, ": ", valueNm, "</p>")))

      } else {
        check_comment_ui2(id, "D3", HTML("Calculated effect on vulnerability:"),
                         choiceNames = valueNms,
                         choiceValues = valueOpts,
                         selected = box_val,
                         com = prevCom,
                         guide = FALSE)
      }

    })

    # This makes sure that the value is updated even if the tab isn't reopened
    outputOptions(output, "box_D3", suspendWhenHidden = FALSE)

    # Return -------------------------------------------------
    list("d" = reactive(collect_questions(input)))

  })

}
