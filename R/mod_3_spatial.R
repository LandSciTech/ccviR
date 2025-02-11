
#' Test the spatial module
#'
#' @noRd
#' @examples
#' mod_spatial_test(input_files = NULL) # Basic, no files
#' mod_spatial_test()                   # With test files pre-loaded
#' mod_spatial_test(df_loaded = TRUE)   # As if re-loading from previous run


mod_spatial_test <- function(df_loaded = FALSE, input_files = test_files()) {

  ui <- ui_setup(mod_spatial_ui(id = "test"))
  server <- function(input, output, session) {
    shinyOptions("file_dir" = "inst/extdata/")

    volumes <- server_setup()
    if(df_loaded) {
      df_loaded <- test_files()$saved$final %>%
        load_previous() %>%
        reactive()
    } else df_loaded <- reactive(NULL)

    mod_spatial_server(id = "test", volumes, df_loaded, cave = reactive(FALSE),
                       parent_session = session, input_files)
  }

  shinyApp(ui, server)
}

mod_spatial_ui <- function(id) {

  ns <- NS(id)

  tabPanel(
    "Spatial Data Analysis",
    fluidRow(
      column(
        12,
        div(
          id = ns("spatial"),
          h2("Spatial Data Analysis"),
          p("The spatial data input in this section will be used to calculate
                  the exposure to climate change (Section A). It will also be used
                  to evaluate select questions in Section C that have a spatial
                  component. If provided, the range change raster(s) will be used to
                  evalaute questions about the modeled response to climate change
                  in Section D. Required datasets are indicated with", labelMandatory("a")),
          get_file_ui2(id, "clim_var_dir", "Folder location of prepared climate data",
                      type = "dir", mandatory = TRUE, spinner = TRUE),
          br(),
          get_file_ui2(id, "rng_poly_pth", "Range polygon shapefile", mandatory = TRUE),
          get_file_ui2(id, "assess_poly_pth", "Assessment area polygon shapefile", mandatory = TRUE),
          get_file_ui2(id, "ptn_poly_pth", "Physiological thermal niche file"),
          get_file_ui2(id, "nonbreed_poly_pth", "Non-breeding Range polygon shapefile"),
          selectInput(ns("rng_chg_used"), "Will a projected range change raster be supplied?",
                      c("No" = "no",
                        "Yes, one range change raster will be supplied for all scenarios" = "one",
                        "Yes, multiple range change rasters will be supplied, one for each scenario (Preferred)" = "multiple")),
          uiOutput(ns("rng_chg_sel_ui")),
          verbatimTextOutput(ns("rng_chg_error")),
          conditionalPanel(
            condition = "input.rng_chg_used !== 'no'",
            strong("Classification of projected range change raster"),
            p("Enter the range of values in the raster corresponding to ",
              "lost, maintained, gained and not suitable."),
            # TODO: Replace with sliders?
            from_to_ui2(id, "lost", "Lost:",  c(-1, -1)),
            from_to_ui2(id, "maint", "Maintained:", c(0, 0)),
            from_to_ui2(id, "gain", "Gained:", c(1,1)),
            from_to_ui2(id, "ns", "Not Suitable:", c(99, 99)),
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
            numericInput(ns("gain_mod"), NULL, 1, min = 0, max = 1, step = 0.1),
            textAreaInput(ns("gain_mod_comm"), "Gain modifier explanation"),
            ns = NS(id)
          ),
          br(),
          h5("Click button to begin the spatial analysis or to re-run it",
             " after changing inputs:"),
          shinyjs::disabled(
            actionButton(ns("startSpatial"), "Run Spatial Analysis", class = "btn-primary")),
          #br(),
          #conditionalPanel(
          #  condition = "input.startSpatial > 0",
          #  shinycssloaders::withSpinner(verbatimTextOutput(ns("spat_error")),
          #                               proxy.height = "50px"),
          #  ns = NS(id)
          #),
          br(),br(),
          actionButton(ns("continue"), "Next", class = "btn-primary"),
          br(), br(),
          # this hidden input will allow us to stop processing until returning
          # to the UI so that values from the saved file are updated in input
          # before using
          div(style = "display:none", textInput(inputId = ns("hidden"), label = "", value = ""))
        )
      )
    )
  )

}

mod_spatial_server <- function(id, volumes, df_loaded, cave, parent_session,
                               input_files = NULL) {

  stopifnot(is.reactive(df_loaded))
  stopifnot(is.reactive(cave))

  is_shiny_testing()
  if(is.null(input_files) & is_shiny_testing()) {
    input_files <- test_files()
  }

  moduleServer(id, function(input, output, session) {

    # Setup ----------------------

    ns <- session$ns

    # Values
    repeatSpatial <- reactiveVal(FALSE)

    spat_res <- reactiveVal(FALSE)
    spat_res2 <- reactiveVal(FALSE)
    doSpatial <- reactiveVal(0)
    doSpatialRestore <- reactiveVal(FALSE)

    # Catch changes to dir/file paths from either loading previous or inputs
    # Need to be reactiveVal/ues because modified through several different pathways
    file_pths <- reactiveValues() # Prevent individual file paths from depending on each other
    clim_dir_pth <- reactiveVal()
    file_ids <- c("rng_poly_pth", "nonbreed_poly_pth", "assess_poly_pth",
                  "ptn_poly_pth")
    rng_ids <- reactiveVal()

    # Continue Button
    observeEvent(input$continue, switch_tab("Vulnerability Questions - A", parent_session))

    # Enable the Start Spatial button when all mandatory fields are filled out
    observe({
      shinyjs::disable("startSpatial")
      req(rng_poly(), assess_poly(), clim_vars1(), clim_readme())
      shinyjs::enable("startSpatial")
    })

    # Set paths for testing ----------------------
    observe({
      if(!is.null(input_files)) {
        clim_dir_pth(input_files$clim_dir)
        stringr::str_subset(names(input_files), "pth") %>%
          purrr::walk(~{
            file_pths[[.x]] <- input_files[[.x]]
          })
      }
    })

    # Setup ShinyFiles ------------------------------

    # Find File/Dir paths
    purrr::map(file_ids, shinyFileChoose, root = volumes, input = input,
               filetypes = c("shp", "tif", "tiff", "asc", "nc", "grd", "bil"))
    observeEvent(rng_ids(), {
      purrr::map(rng_ids(), shinyFileChoose, root = volumes, input = input,
                 filetypes = c("shp", "tif", "tiff", "asc", "nc", "grd", "bil"))
    })
    shinyDirChoose(input, "clim_var_dir", root = volumes)


    # Restore data ----------------
    observeEvent(df_loaded(), {
      update_restored2(df_loaded(), section = "spatial", session)
    })

    restored_spatial <- eventReactive(
      {
        df_loaded()
        # this is to make it trigger after update_restored
        input$hidden
      },
      {
        # this is to avoid running before input has been updated
        req(input$hidden)

        df_loaded <- df_loaded()
        if(!is.null(df_loaded$MAT_6) & !all(is.na(df_loaded$MAT_6))){
          # need spat tholds to get exp multipliers
          df_spat <- apply_spat_tholds(df_loaded, df_loaded$cave)
          # need use df_loaded for all other values to preserve changes to spat vuln qs
          df_spat2 <- df_loaded %>%
            left_join(df_spat %>%
                        select("scenario_name", setdiff(names(df_spat), names(df_loaded))),
                      by = 'scenario_name')
          spat_res2(df_spat2)
          repeatSpatial(TRUE)
          doSpatial((doSpatial() +1))
          # set to same as doSpatial so can check value and if same don't update spat_res2
          doSpatialRestore(doSpatial())
          showNotification("Re-running spatial analysis from loaded file.",
                           duration = NULL, id = ns("spat_restore_note"))
        }

        loaded_pths <- df_loaded %>%
          slice(1) %>%
          select(contains("pth"), -any_of("clim_dir_pth")) %>%
          as.list()

        if(length(loaded_pths)>0) {
          # TODO: FIX!
          file_pths <- purrr::discard(loaded_pths, is.na)
        }

        clim_pth_ldd <- df_loaded %>% slice(1) %>% pull(.data$clim_dir_pth)
        clim_pth_ldd <- ifelse(is.na(clim_pth_ldd), "", clim_pth_ldd)
        clim_dir_pth(clim_pth_ldd)

        switch_tab("Species Information", parent_session)

        return(TRUE)
      })


    observeEvent(restored_spatial(), {
      if (restored_spatial()){
        showNotification("Successfully restored from file.", duration = 10)
      } else {
        showNotification("CSV file is invalid. Failed to restore from file.", duration = 10)
      }
    })



    # UI -------------------------

    # use readme to render scenario names for rng chg rasters
    output$rng_chg_sel_ui <- renderUI({

      # Remove any old rng_chg_pths values
      isolate({
        nms_old <- stringr::str_subset(names(input), "rng_chg_pth$|rng_chg_pth_\\d")
        if(length(nms_old) > 0) {
          purrr::walk(nms_old, ~{
            file_pths[[.x]] <- ""
          })
        }
      })

      if(input$rng_chg_used == "no"){
        rng_ids(NULL)
        return(NULL)
      } else if(input$rng_chg_used == "one"){
        rng_ids("rng_chg_pth")
        get_file_ui2(id, "rng_chg_pth", "Projected range change raster")
      } else if (input$rng_chg_used == "multiple"){
        validate(need(
          is_ready(clim_readme()),
          paste0("Require climate readme to proceed: ",
                 "Please select a valid Climate data folder above")
        ))
        # Track rng_chg_pth inputs
        rng_ids(paste0("rng_chg_pth_", seq_along(clim_readme()$Scenario_Name)))
        # Create inputs
        tagList(
          strong("Select a projected range change raster for each scenario"),
          purrr::imap(clim_readme()$Scenario_Name,
                      ~get_file_ui2(id, paste0("rng_chg_pth_", .y), .x))
        )

      }
    })

    # Dealing with files --------------


    # Parse File paths
    # - make parsing files independent for each file so cleared file names are not
    #   retrieved by parse
    purrr::walk(
      file_ids,
      ~ observeEvent(input[[.x]], {
        if(!is.integer(input[[.x]])) {
          pth_in <- parseFilePaths(volumes, input[[.x]])$datapath
          file_pths[[.x]] <- pth_in
        }
      }, ignoreInit = TRUE)
    )

    observeEvent(
      rng_ids(), {
        purrr::walk(
          rng_ids(),
          ~ observeEvent(input[[.x]], {
            if(!is.integer(input[[.x]])) {
              pth_in <- parseFilePaths(volumes, input[[.x]])$datapath
              file_pths[[.x]] <- pth_in
            }
          }, ignoreInit = TRUE)
        )
      })


    # Parse Dir paths
    observeEvent(input$clim_var_dir, {
      if(!is.integer(input$clim_var_dir)) {
        clim_dir_pth(parseDirPath(volumes, input$clim_var_dir))
      }
    }, ignoreInit = TRUE)


    # Clear File paths when x clicked
    purrr::walk(
      paste0(file_ids, "_clear"), # Button ids
      ~ observeEvent(input[[.x]], {
        if(input[[.x]] > 0) {
          fl_x <- stringr::str_extract(.x, "(.*)(_clear)", group = 1)
          file_pths[[fl_x]] <- ""
        }
      }, ignoreInit = TRUE)
    )

    observeEvent(rng_ids(), {
      purrr::walk(
        paste0(rng_ids(), "_clear"), # Button ids
        ~ observeEvent(input[[.x]], {
          if(input[[.x]] > 0) {
            fl_x <- stringr::str_extract(.x, "(.*)(_clear)", group = 1)
            file_pths[[fl_x]] <- ""
          }
        }, ignoreInit = TRUE)
      )
    })

    # Clear Dir paths when x clicked
    observeEvent(input$clim_var_dir_clear, {
      clim_dir_pth(NULL)
    })

    # Output File paths
    purrr::walk(file_ids, ~{
      output[[paste0(.x, "_out")]] <- renderText(file_pths[[.x]])
    })

    observeEvent(rng_ids(), {
      purrr::walk(rng_ids(), ~{
        output[[paste0(.x, "_out")]] <- renderText(file_pths[[.x]])
      })
    })

    # Output Dir paths
    output$clim_var_dir_out <- renderText({
      clim_dir_pth()
    })

    # Load Spatial data -------------------

    # Polygons
    rng_poly <- reactive(read_poly(file_pths$rng_poly_pth, "Range Polygon", req = TRUE))
    assess_poly <- reactive(read_poly(file_pths$assess_poly_pth, "Assessment Polygon", req = TRUE))
    nonbreed_poly <- reactive(read_poly(file_pths$nonbreed_poly_pth, "Non-breeding Polygon"))
    ptn_poly <- reactive(read_poly(file_pths$ptn_poly_pth, "PTN Polygon"))

    # Raster
    rng_chg <- reactive({
      req(rngs <- rng_ids())

      # Wait until we have all the files before loading
      purrr::map(rngs, ~req(file_pths[[.]]))

      # Load files into one raster
      purrr::map(rngs, ~file_pths[[.x]]) %>%
        stats::setNames(rngs) %>%
        read_raster(scn_nms = clim_readme()$Scenario_Name, "Projected Range Changes")
    })

    # Climate data
    clim_readme <- reactive({
      req(clim_dir_pth())
      read_clim_readme(clim_dir_pth())

    })

    clim_vars1 <- reactive({
      req(clim_readme())
      read_clim(clim_dir_pth(), clim_readme()$Scenario_Name)
    })

    # Matrix
    rng_chg_mat <- reactive({
      mat <- matrix(c(input$lost_from, input$lost_to, 1,
                      input$maint_from, input$maint_to, 2,
                      input$gain_from, input$gain_to, 3,
                      input$ns_from, input$ns_to, 0),
                    byrow = TRUE, ncol = 3)

      # if an input is blank then the value is NA but that converts raster values that
      # are NA to that value
      mat[which(!is.na(mat[, 1])), ]

    })

    # Catch Loading Errors ---------------------------------------------

    # Create error text boxes for dir input
    output$clim_var_dir_error <- renderText({
      pth <- fs::path(clim_dir_pth(), "climate_data_readme.csv")
      req(pth)
      validate(need(
        fs::file_exists(pth),
        "The climate folder is missing the required 'climate_data_readme.csv' file"))

      validate(need(!inherits(clim_vars1(), "try-error"), "Could not load climatic variables"))

      if(inherits(clim_vars1(), "try-error")){
        stop(conditionMessage(attr(clim_vars1(), "condition")))
      }
    })

    # Create error text boxes for all file inputs
    output$rng_poly_pth_error <- renderText({
      req(rng_poly())
      invisible()
    })

    output$assess_poly_pth_error <- renderText({
      req(assess_poly())
      invisible()
    })

    output$ptn_poly_pth_error <- renderText({
      req(ptn_poly())
      invisible()
    })

    output$nonbreed_poly_pth_error <- renderText({
      req(nonbreed_poly())
      invisible()
    })

    output$rng_chg_error <- renderText({
      req(rng_chg())
      invisible()
    })

    # output$rng_poly_pth_error <- renderText({
    #  req(rng_poly())
    # })


    # Run Spatial Analysis --------------------------------------------------

    # run spatial calculations
    spat_res1 <- eventReactive(doSpatial(), {
      req(doSpatial())
      req(clim_vars1())
      if(getOption("ccviR.debug")) message("Running Spatial Analyses")
      withProgress(message = "Running Spatial Analyses", {
        out <- tryCatch({
          analyze_spatial(range_poly = rng_poly(),
                          non_breed_poly = nonbreed_poly(),
                          scale_poly = assess_poly(),
                          hs_rast = rng_chg(),
                          ptn_poly = ptn_poly(),
                          clim_vars_lst = clim_vars1(),
                          hs_rcl = rng_chg_mat(),
                          gain_mod = input$gain_mod,
                          scenario_names = clim_readme()$Scenario_Name)
        },
        error = function(cnd) conditionMessage(cnd))
      })

      removeNotification(ns("spat_restore_note"))
      return(out)

    }, ignoreInit = TRUE)


    observeEvent(input$startSpatial, {
      showModal(modalDialog(
        p("Note: Re-running the spatial analysis will overwrite any changes made to ",
          "the Spatial Vulnerability Questions. Comments will be preserved so ",
          "you can record the change made in the comments and then change it ",
          "again after re-running the analysis."),
        footer = tagList(
          actionButton(ns("shinyalert"), "Continue"),
          modalButton("Cancel")
        ),
        title = "Do you want to run the spatial analysis?"))

      # Remove if first run
      # TODO: Can we just skip the modal if !repeatSpatial()?
      if(!repeatSpatial()) shinyjs::click("shinyalert")
    })

    observeEvent(input$shinyalert, {
      removeModal()
      if(input$shinyalert > 0){
        doSpatial(doSpatial() + 1)
        repeatSpatial(TRUE)
      }
      shinyjs::runjs("window.scrollTo(0, document.body.scrollHeight)")
    })

    range_poly_clip <- reactive({
      req(rng_poly())
      req(doSpatial())
      req(!is.character(spat_res1()))
      spat_res1()$range_poly_assess
    })

    range_poly_clim <- reactive({
      req(doSpatial())
      req(!is.character(spat_res1()))
      spat_res1()$range_poly_clim
    })

    observe({
      req(doSpatial())
      req(!is.character(spat_res1()))
      spat_res(spat_res1()$spat_table)
    })


    # output$spat_error <- renderText({
    #   if(inherits(rng_chg(), "try-error")){
    #     stop("Error in range change raster",
    #          conditionMessage(attr(rng_chg(), "condition")))
    #   }
    #   if(is.character(spat_res1())){
    #     stop(spat_res1(), call. = FALSE)
    #   } else {
    #     "Spatial analysis complete"
    #   }
    # })

    # calculate exp multipliers and vuln Q values for spat
    observeEvent(spat_res(), {
      req(!is.character(spat_res()))
      req(spat_res())
      req(!doSpatial() == doSpatialRestore())
      message("updateing spat_res2")
      spat_res2(apply_spat_tholds(spat_res(), cave()))

    })

    # Prepare Spatial outputs ----------------------------------------------

    # TODO: Original was an observeEvent which linked to the out_data_lst reactiveValue
    # Check that this is good
    spatial_data <- eventReactive(spat_res(), {
      req(spat_res())
      req(clim_readme())

      spat_df <- spat_res() %>%
        mutate(gain_mod = input$gain_mod,
               gain_mod_comm = input$gain_mod_comm,
               lost = paste0(input$lost_from, ", ", input$lost_to),
               maint = paste0(input$maint_from, ", ", input$maint_to),
               gain = paste0(input$gain_from, ", ", input$gain_to),
               ns = paste0(input$ns_from, ", ", input$ns_to),
               rng_chg_used = input$rng_chg_used)
      clim_rdme <- clim_readme() %>% select(-"Scenario_Name", -contains("brks"))

      spat_fnms <- lapply(file_pths, function(x) ifelse(is.null(x),"",x)) %>%
        as.data.frame() %>%
        mutate(clim_dir_pth = clim_dir_pth())

      bind_cols(
        spat_df %>% select(-any_of(c(colnames(clim_rdme),
                                     colnames(spat_fnms)))),
        clim_rdme, spat_fnms)
    })

    # # Return -------------------------------------------------
    # exportTestValues(
    #   "spatial_data" = spatial_data(),
    #   "spatial_details" = list(
    #     "spat_res" = spat_res2(),
    #     "clim_vars" = clim_vars1(),
    #     "clim_readme" = clim_readme(),
    #     "range_poly" = range_poly_clip(),
    #     "range_poly_clim" = range_poly_clim(),
    #     "ptn_poly" = ptn_poly(),
    #     "nonbreed_poly" = nonbreed_poly(),
    #     "assess_poly" = assess_poly(),
    #     "hs_rast" = rng_chg(),
    #     "hs_rcl_mat" = rng_chg_mat()
    #   ))

    list("spatial_data" = spatial_data,
         "spatial_details" = list(
           "spat_res" = spat_res2,
           "clim_vars" = clim_vars1,
           "clim_readme" = clim_readme,
           "range_poly" = range_poly_clip,
           "range_poly_clim" = range_poly_clim,
           "ptn_poly" = ptn_poly,
           "nonbreed_poly" = nonbreed_poly,
           "assess_poly" = assess_poly,
           "hs_rast" = rng_chg,
           "hs_rcl_mat" = rng_chg_mat
         ))
  })

}
