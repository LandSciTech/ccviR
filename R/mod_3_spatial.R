
#' Test the spatial module
#'
#' @noRd
#' @examples
#' mod_spatial_test()
#' mod_spatial_test(df_loaded = TRUE)

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
          verbatimTextOutput(ns("clim_var_error")),
          br(),
          get_file_ui2(id, "range_poly_pth", "Range polygon shapefile", mandatory = TRUE),
          get_file_ui2(id, "assess_poly_pth", "Assessment area polygon shapefile", mandatory = TRUE),
          get_file_ui2(id, "ptn_poly_pth", "Physiological thermal niche file"),
          get_file_ui2(id, "nonbreed_poly_pth", "Non-breeding Range polygon shapefile"),
          selectInput(ns("rng_chg_used"), "Will a projected range change raster be supplied?",
                      c("No" = "no",
                        "Yes, one range change raster will be supplied for all scenarios" = "one",
                        "Yes, multiple range change rasters will be supplied, one for each scenario (Preferred)" = "multiple")),
          uiOutput(ns("rng_chg_sel_ui")),
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
          actionButton(ns("startSpatial"), "Run Spatial Analysis", class = "btn-primary"),
          br(),
          conditionalPanel(
            condition = "input.startSpatial > 0",
            shinycssloaders::withSpinner(verbatimTextOutput(ns("spat_error")),
                                         proxy.height = "50px"),
            ns = NS(id)
          ),
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
    clim_vars <- reactiveVal()
    range_poly_in <- reactiveVal()
    repeatSpatial <- reactiveVal(FALSE)
    nonbreed_poly <- reactiveVal()
    assess_poly <- reactiveVal()
    hs_rast <- reactiveVal()
    ptn_poly <- reactiveVal()
    hs_rcl_mat <- reactiveVal()

    spat_res <- reactiveVal(FALSE)
    spat_res2 <- reactiveVal(FALSE)
    doSpatial <- reactiveVal(0)
    doSpatialRestore <- reactiveVal(FALSE)

    file_pths <- reactiveVal()
    clim_dir_pth <- reactiveVal()

    # Continue Button
    observeEvent(input$continue, switch_tab("Vulnerability Questions - A", parent_session))

    # Enable the Start Spatial button when all mandatory fields are filled out
    observe({
      filled <-
        vapply(c("range_poly_pth", "assess_poly_pth"),
               function(x) {
                 isTruthy(file_pths()[[x]]) & isTruthy(clim_dir_pth())
               },
               logical(1)) |>
        all()
      if (is_shiny_testing()) {
        filled <- TRUE
      }

      shinyjs::toggleState(id = "startSpatial", condition = filled)
    })

    # Set paths for testing ----------------------
    observe({
      if(!is.null(input_files)) {
        clim_dir_pth(input_files$clim_dir)

        pths <- list(
          range_poly_pth = input_files$rng_poly_pth,
          assess_poly_pth = input_files$assess_poly_pth,
          ptn_poly_pth = input_files$ptn_poly_pth,
          rng_chg_pths = c(input_files$rng_chg_pth_1,
                           input_files$rng_chg_pth_2)
        )

        file_pths(pths)
      }
    })



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

        if(length(loaded_pths)>0){
          file_pths(purrr::discard(loaded_pths, is.na))
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

    # Dealing with files --------------

    # Parse File paths
    # - make parsing files independent for each file so cleared file names are not
    #   retrieved by parse
    observe({
      purrr::walk(
        filePathIds(),
        ~ observeEvent(input[[.x]], {
          if(!is.integer(input[[.x]])) {
            pths_in <- file_pths()
            pths_in[[.x]] <- parseFilePaths(volumes, input[[.x]])$datapath
            file_pths(pths_in)
          }
        }, ignoreInit = TRUE))
    })

    # Parse Dir paths
    observeEvent(input$clim_var_dir, {
      if(!is.integer(input$clim_var_dir)) {
        clim_dir_pth(parseDirPath(volumes, input$clim_var_dir))
      }
    }, ignoreInit = TRUE)


    # Clear File paths when x clicked
    observe({
      buttonIds <- paste0(filePathIds(), "_clear")
      purrr::walk(
        buttonIds,
        ~ observeEvent(input[[.x]], {
          if(input[[.x]] > 0) {
            pths_in <- file_pths()
            fl_x <- stringr::str_extract(.x, "(.*)(_clear)", group = 1)
            pths_in[[fl_x]] <- ""
            file_pths(pths_in)
          }
        }, ignoreInit = TRUE))
    })

    # Clear Dir paths when x clicked
    observeEvent(input$clim_var_dir_clear, {
      clim_dir_pth(NULL)
    })

    # Output File paths
    observe({
      purrr::walk2(file_pths(), filePathIds()[names(file_pths())], ~{
        out_name <- paste0(.y, "_out")
        output[[out_name]] <- renderText({.x})
      })
    })

    # Output Dir paths
    output$clim_var_dir_out <- renderText({
      clim_dir_pth()
    })


    # update filePathIds based on selection for rng_chg
    filePathIds <- reactive({
      # File path ids to use with file choose
      fileIds <- c("range_poly_pth", "nonbreed_poly_pth", "assess_poly_pth", "ptn_poly_pth")
      names(fileIds) <- fileIds

      rng_chg_pths <- stringr::str_subset(names(input), "rng_chg_pth_\\d$|rng_chg_pth$")

      if(length(rng_chg_pths) > 0){
        names(rng_chg_pths) <- rng_chg_pths

        return(c(fileIds, rng_chg_pths))
      } else {
        return(fileIds)
      }

    })

    # Find File/Dir paths
    observe({
      purrr::map(filePathIds(), shinyFileChoose, root = volumes, input = input,
                 filetypes = c("shp", "tif", "tiff", "asc", "nc", "grd", "bil"))
    })
    shinyDirChoose(input, "clim_var_dir", root = volumes)


    # Load Spatial data -------------------

    clim_readme <- reactive({
      req(clim_dir_pth())

      if(!file.exists(fs::path(clim_dir_pth(), "climate_data_readme.csv"))){
        stop("The climate folder is missing the required climate_data_readme.csv file",
             call. = FALSE)
      }
      utils::read.csv(fs::path(clim_dir_pth(), "climate_data_readme.csv"),
                      check.names = FALSE)
    })

    clim_vars1 <- reactive({
      req(clim_readme())

      clim_vars_out <- try(
        get_clim_vars(clim_dir_pth(), scenario_names = clim_readme()$Scenario_Name)
      )
      clim_vars_out
    })


    observeEvent(doSpatial(), {
      clim_vars(clim_vars1())
    })

    observeEvent(doSpatial(), {
      range_poly_in(sf::st_read(file_pths()$range_poly_pth, agr = "constant", quiet = TRUE))
    }, ignoreInit = TRUE)


    observeEvent(doSpatial(), {
      pth <- file_pths()$nonbreed_poly_pth

      if(!isTruthy(pth)){
        return(NULL)
      }
      nonbreed_poly(sf::st_read(pth, agr = "constant", quiet = TRUE))
    }, ignoreInit = TRUE)


    observeEvent(doSpatial(), {
      pol <- file_pths()$assess_poly_pth %>%
        sf::st_read(agr = "constant", quiet = TRUE) %>%
        valid_or_error("assessment area polygon")
      assess_poly(pol)
    }, ignoreInit = TRUE)

    # use readme to render scenario names for rng chg rasters
    output$rng_chg_sel_ui <- renderUI({
      if(input$rng_chg_used == "no"){
        return(NULL)
      } else if(input$rng_chg_used == "one"){
        get_file_ui2(id, "rng_chg_pth", "Projected range change raster")
      } else if (input$rng_chg_used == "multiple"){
        tagList(
          strong("Select a projected range change raster for each scenario"),
          purrr::map2(clim_readme()$Scenario_Name,
                      1:length(clim_readme()$Scenario_Name),
                      ~get_file_ui2(id, paste0("rng_chg_pth", "_", .y), .x)),
          br(), br()
        )

      }
    })

    observeEvent(input$rng_chg_used, {
      # check for names of old rng_chg_pths
      nms_old <- stringr::str_subset(names(input), "rng_chg_pth$|rng_chg_pth_\\d")
      if(length(nms_old) > 0){
        purrr::walk(nms_old, \(x){
          pths_in <- file_pths()
          pths_in[[x]] <- ""
          file_pths(pths_in)
        })

      }
    }, ignoreInit = TRUE)
    # doing this rather than eventReactive so that it still has a value (NULL)
    # if shinyalert is not called


    observeEvent(doSpatial(), {
      pth <- file_pths()[stringr::str_subset(names(file_pths()), "rng_chg_pth")]
      pth <- unlist(pth)
      pth <- pth[sort(names(pth))]

      if(!isTruthy(pth) || length(pth) == 0){
        hs_rast(NULL)
      } else {
        names(pth) <- fs::path_file(pth) %>% fs::path_ext_remove()
        message("loading rng_chg_rasts")
        out <- check_trim(terra::rast(pth))
        terra::set.names(out, clim_readme()$Scenario_Name)
        hs_rast(out)
      }
    }, ignoreInit = TRUE)

    observeEvent(doSpatial(), {
      pth <- file_pths()$ptn_poly_pth

      if(!isTruthy(pth)){
        ptn_poly(NULL)
      } else {
        ptn_poly(sf::st_read(pth, agr = "constant", quiet = TRUE))
      }
    }, ignoreInit = TRUE)

    # assemble hs_rcl matrix

    observeEvent(doSpatial(), {
      mat <- matrix(c(input$lost_from, input$lost_to, 1,
                      input$maint_from, input$maint_to, 2,
                      input$gain_from, input$gain_to, 3,
                      input$ns_from, input$ns_to, 0),
                    byrow = TRUE, ncol = 3)

      # if an input is blank then the value is NA but that converts raster values that
      # are NA to that value
      hs_rcl_mat(mat[which(!is.na(mat[, 1])), ])
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

    # run spatial calculations
    spat_res1 <- eventReactive(doSpatial(), {
      req(doSpatial())
      req(clim_vars())
      out <- tryCatch({
        analyze_spatial(range_poly = range_poly_in(),
                        non_breed_poly = nonbreed_poly(),
                        scale_poly = assess_poly(),
                        hs_rast = hs_rast(),
                        ptn_poly = ptn_poly(),
                        clim_vars_lst = clim_vars(),
                        hs_rcl = hs_rcl_mat(),
                        gain_mod = input$gain_mod,
                        scenario_names = clim_readme()$Scenario_Name)
      },
      error = function(cnd) conditionMessage(cnd))

      # force these to invalidate when re-run
      spat_res(FALSE)

      removeNotification(ns("spat_restore_note"))
      return(out)

    }, ignoreInit = TRUE)

    range_poly <- reactive({
      req(range_poly_in())
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

    output$clim_var_error <- renderText({
      if(inherits(clim_vars1(), "try-error")){
        stop(conditionMessage(attr(clim_vars1(), "condition")))
      }
    })

    output$spat_error <- renderText({
      if(inherits(hs_rast(), "try-error")){
        stop("Error in range change raster",
             conditionMessage(attr(hs_rast(), "condition")))
      }
      if(is.character(spat_res1())){
        stop(spat_res1(), call. = FALSE)
      } else {
        "Spatial analysis complete"
      }
    })

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
      req(!is.null(file_pths()))

      spat_df <- spat_res() %>%
        mutate(gain_mod = input$gain_mod,
               gain_mod_comm = input$gain_mod_comm,
               lost = paste0(input$lost_from, ", ", input$lost_to),
               maint = paste0(input$maint_from, ", ", input$maint_to),
               gain = paste0(input$gain_from, ", ", input$gain_to),
               ns = paste0(input$ns_from, ", ", input$ns_to),
               rng_chg_used = input$rng_chg_used)
      clim_rdme <- clim_readme() %>% select(-"Scenario_Name", -contains("brks"))
      spat_fnms <- lapply(file_pths(), function(x) ifelse(is.null(x),"",x)) %>%
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
    #     "clim_vars" = clim_vars(),
    #     "clim_readme" = clim_readme(),
    #     "range_poly" = range_poly(),
    #     "range_poly_clim" = range_poly_clim(),
    #     "ptn_poly" = ptn_poly(),
    #     "nonbreed_poly" = nonbreed_poly(),
    #     "assess_poly" = assess_poly(),
    #     "hs_rast" = hs_rast(),
    #     "hs_rcl_mat" = hs_rcl_mat()
    #   ))

    list("spatial_data" = spatial_data,
         "spatial_details" = list(
           "spat_res" = spat_res2,
           "clim_vars" = clim_vars,
           "clim_readme" = clim_readme,
           "range_poly" = range_poly,
           "range_poly_clim" = range_poly_clim,
           "ptn_poly" = ptn_poly,
           "nonbreed_poly" = nonbreed_poly,
           "assess_poly" = assess_poly,
           "hs_rast" = hs_rast,
           "hs_rcl_mat" = hs_rcl_mat
         ))
  })

}
