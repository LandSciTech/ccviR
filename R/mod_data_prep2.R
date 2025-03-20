#' Launch the data preparation app
#'
#' Launch the data preparation app for the ccviR package. See
#' `vignette("data_prep_vignette", package = "ccviR")` for details on how to use
#' the app.
#'
#' @param file_dir The directory to locate files from or "demo" to use the demo
#'   data included in the package.
#' @param launch.browser logical. Run app in browser?
#' @param port If launch.browser is FALSE, specify port to run CCVI app.
#' @param test.mode Should the app be launched using shiny test.mode. Only set
#'   to TRUE for debugging.
#'
#' @export
#'
#' @returns A shiny app.
#'
#' @examplesIf interactive()
#'  run_data_prep2("demo")

run_data_prep2 <- function(file_dir = getwd(),
                           launch.browser = TRUE,
                           port = getOption("shiny.port")){

  if(file_dir == "demo"){
    file_dir <- system.file("extdata", package = "ccviR")
  }
  shiny::shinyOptions(file_dir = file_dir)

  ui <- fluidPage(
    title = "Data Preparation for the ccviR app",
    ui_fmt(type = "data-ui"),
    mod_data_prep_ui(id = "data")
  )

  server <- function(input, output, session) {
    mod_data_prep_server(id = "data")
  }

  shinyApp(ui, server, options = list(launch.browser, port))
}

#' Test the data prep module
#'
#' @noRd
#' @examples
#' # Test with all inputs pre-filled
#' mod_data_prep_test()
#'
#' # Test with vanilla
#' mod_data_prep_test(input_files = NULL)

mod_data_prep_test <- function(input_files = test_data_prep()) {
  ui <- fluidPage(
    title = "Data Preparation for the ccviR app",
    ui_fmt(type = "data-ui"),
    mod_data_prep_ui(id = "data")
  )

  server <- function(input, output, session) {
    shinyOptions("file_dir" = "inst/extdata/")
    mod_data_prep_server(id = "data", input_files)
  }

  shinyApp(ui, server)
}


mod_data_prep_ui <- function(id) {

  ns <- NS(id)

  fluidRow(
    style = "padding-left: 5rem; padding-right: 5rem;",

    h2("Prepare data for the CCVI App"),
    p(em("NOTE: Running this app is only necessary if you want to run the ",
         "CCVI app with custom climate data. To use the pre-prepared climate ",
         "data download a zip file from ",
         a("here", href = "https://drive.google.com/drive/folders/18mO5GDUmwi-nswhIAC36bmtYsvmqNQkH?usp=share_link", target="_blank", .noWS = "after"),
         ", unzip it and save the folder in a convenient location.")),
    p(strong("Step 1: "), "Acquire climate data including: ",
      "Mean annual temperature (MAT), climate moisture deficeit (CMD), ",
      "mean annual precipitation (MAP), and minimum coldest and ",
      "maximum warmest month temperatures (MCMT, MWMT) for a historical period",
      "and MAT and CMD for a future period. ",
      "To calculate a migratory exposure index acquire a climate change ",
      "exposure index (CCEI) raster for the migratory region. ",
      "See the ", a("tutorial", href = "https://landscitech.github.io/ccviR/articles/data_prep_vignette.html"),
      " for recommended data sources.",
      "Save the downloaded data in a folder you can easily find. "),
    p(strong("NOTE:"),
      "When processing multiple climate scenarios the first scenario ",
      "processed will determine the thresholds used to classify the ",
      "temperature and moisture exposure based on the median and 1/2 the ",
      "interquartile range. All scenarios must be processed in one session ",
      "for the correct thresholds to be applied.",
      "Repeat steps 2 and 3 for each scenario."),

    p(strong("Step 2:"), "Record a description of the climate data. ",
      "The Sceanrio Name should be short and an appropriate title for results. ",
      "It will be used as a suffix to the saved output data and to identify the scenario."),

    p(strong("Step 3: "), "Prepare the climate data for use in the app.",
      "Climate data can be added by selecting file paths for each file. ",
      "For the output folder make sure to choose a location that is easy to find again ",
      "because you will use the prepared climate data to calculate the index.",
      "The output folder should be the same for all scenarios."),


    h3("Setup"),

    get_file_ui2(id, "out_folder", "Output folder", mandatory = TRUE,
                 type = "dir"),
    checkboxInput(ns("allow_over"), "Overwrite existing files?"),

    h3("Historical data"),

    textInput(ns("clim_norm_period"), labelMandatory("Historical normal period")),
    textInput(ns("clim_norm_url"), labelMandatory("Link to Source")),

    get_file_ui2(id, "mat_norm_pth", "Historical mean annual temperature", TRUE),
    get_file_ui2(id, "cmd_norm_pth", "Historical climatic mositure deficit", TRUE),
    get_file_ui2(id, "map_norm_pth", "Historical mean annual precipitation"),
    get_file_ui2(id, "mwmt_norm_pth", "Historical mean warmest month temperature"),
    get_file_ui2(id, "mcmt_norm_pth", "Historical mean coldest month temperature"),

    h3("Supporting data"),
    # TODO: Assessment area?
    get_file_ui2(id, "assess_pth", "Assessment area (Climate data extent polygon)"),
    get_file_ui2(id, "ccei_pth", "Climate change exposure index"),

    h3("Future scenario data"),
    numericInput(ns("scn_n"), label = "How many future scenarios?",
                 value = 1, min = 1, max = 5),
    uiOutput(ns("ui_scenarios")),

    br(),
    actionButton(ns("submit"), "Process", class = "btn-primary"),
    br(),
    p(verbatimTextOutput(ns("submit_error"))),
    br(),
    shinycssloaders::withSpinner(verbatimTextOutput("data_prep_msg",
                                                    placeholder = TRUE)),
    #actionButton("data_reset", "Add Another Scenario"),
    br(),
    actionButton("data_done", "Close", class = "btn-primary")
  )
}


mod_data_prep_server <- function(id, input_files = NULL) {

  volumes <- server_setup()

  moduleServer(id, function(input, output, session) {

    # Setup ----------------------------------------------------------------

    # TODO DELETE BECAUSE I DONT think necessary any more
    # observeEvent(input$data_reset,{
    #   # File path ids to use with file choose
    #   c("clim_scn_nm", "clim_fut_period", "clim_em_scenario",
    #     "mat_fut_pth_out", "cmd_fut_pth_out", "ccei_pth_out",
    #     "map_pth_out", "mwmt_pth_out", "mcmt_pth_out", "assess_pth") %>%
    #     purrr::map(~paste0("data_prep_mod-", .x)) %>%
    #     purrr::map(shinyjs::reset)
    #
    #   shinyjs::runjs("window.scrollTo(0, 0)")
    # })

    # Paths
    # Catch changes to dir/file paths from either loading tests files or inputs
    # reactiveVal/ues, not reactive, bc modified through several pathways
    file_pths <- reactiveValues() # Prevent individual file paths from depending on each other
    file_scn_ids <- reactiveVal()
    out_dir <- reactiveVal()
    file_ids <- c("mat_norm_pth", "cmd_norm_pth",
                  "ccei_pth", "map_pth",
                  "mwmt_pth", "mcmt_pth", "assess_pth")


    # Set inputs/paths for testing ----------------------

    observe({
      if(!is.null(input_files)) {
        req(input$scn_n)

        # Set Inputs
        stringr::str_subset(names(input_files), "pth", negate = TRUE) %>%
          purrr::walk(~{
            updateTextInput(session, .x, value = input_files[[.x]])
          })
        updateCheckboxInput(session, "allow_over", value = TRUE)

        # Set paths
        out_dir("TESTING DATA UI OUTPUTS")
        fs::dir_create(out_dir())
        stringr::str_subset(names(input_files), "pth") %>%
          purrr::walk(~{
            file_pths[[.x]] <- input_files[[.x]]
          })
      }
    })

    # Setup ShinyFiles ------------------------------

    # Find File/Dir paths
    shinyFiles::shinyDirChoose(input, "out_folder", root = volumes)
    purrr::map(
      file_ids, shinyFileChoose, root = volumes, input = input,
      filetypes = c("shp", "tif", "tiff", "asc", "nc", "grd", "bil", ".img"))
    observeEvent(file_scn_ids(), {
      purrr::map(
        file_scn_ids(), shinyFileChoose, root = volumes, input = input,
        filetypes = c("shp", "tif", "tiff", "asc", "nc", "grd", "bil",".img"))
    })

    # Scenarios -----------------------------------
    output$ui_scenarios <- renderUI({

      # Get file name ids for each scenario
      file_scn_ids(paste0(c("mat_fut_pth", "cmd_fut_pth"), input$scn_n))

      # Create inputs
      tabsetPanel(!!!purrr::map(seq_len(input$scn_n), ~ui_scn(id, .x)))
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
      file_scn_ids(), {
        purrr::walk(
          file_scn_ids(),
          ~ observeEvent(input[[.x]], {
            if(!is.integer(input[[.x]])) {
              pth_in <- parseFilePaths(volumes, input[[.x]])$datapath
              file_pths[[.x]] <- pth_in
            }
          }, ignoreInit = TRUE)
        )
      })


    # Parse Dir paths
    observeEvent(input$out_folder, {
      if(!is.integer(input$out_folder)) {
        out_dir(parseDirPath(volumes, input$out_folder))
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

    observeEvent(file_scn_ids(), {
      purrr::walk(
        paste0(file_scn_ids(), "_clear"), # Button ids
        ~ observeEvent(input[[.x]], {
          if(input[[.x]] > 0) {
            fl_x <- stringr::str_extract(.x, "(.*)(_clear)", group = 1)
            file_pths[[fl_x]] <- ""
          }
        }, ignoreInit = TRUE)
      )
    })

    # Clear Dir paths when x clicked
    observeEvent(input$out_folder_clear, {
      out_dir(NULL)
    })

    # Output File paths
    purrr::walk(file_ids, ~{
      output[[paste0(.x, "_out")]] <- renderText(file_pths[[.x]])
    })

    observeEvent(file_scn_ids(), {
      purrr::walk(file_scn_ids(), ~{
        output[[paste0(.x, "_out")]] <- renderText(file_pths[[.x]])
      })
    })

    # Output Dir paths
    output$out_folder_out <- renderText({
      out_dir()
    })


    # Check requirements ------------------------------------
    required_inputs <- reactive({

      # Determine required inputs
      not_required <- c("map_norm_pth", "mwmt_norm_pth", "mcmt_norm_pth",
                        "assess_pth", "ccei_pth")
      req_pths <- purrr::map(
        stats::setNames(nm = c(names(file_pths)[!names(file_pths) %in% not_required])),
        ~file_pths[[.x]])

      req_static <- purrr::map(
        stats::setNames(nm = c("allow_over", "clim_norm_period", "clim_norm_url")),
        ~input[[.x]])

      req_dir <- c("out_dir" = out_dir())

      c(req_pths, req_static, req_dir)
    })

    # NOTE:
    # Could create error text boxes for all file inputs (cf mod_spatial),
    # but don't, as this is wordy and redundant, use the submit_error
    # validate(need()) instead (below).

    # Toggle button ---------------------------
    # Enable the Submit button when all mandatory fields are filled out
    output$submit_error <- renderText({

      shinyjs::disable("submit")

      have_inputs <- purrr::map_lgl(required_inputs(), ~!is.null(.x) & .x != "")
      validate(need(all(have_inputs), "Missing required inputs"))

      shinyjs::enable("submit")
    })


    # Prepare Climate Data --------------------------

    observeEvent(input$submit, {
 browser()
      brks <- list(brks_mat = NULL, brks_cmd = NULL, brks_ccei = NULL)

      message("Processing data")
      brks_out <- prep_clim_data(
        file_pths$mat_norm_pth,
        file_pths$mat_fut_pth,
        file_pths$cmd_norm_pth,
        file_pths$cmd_fut_pth,
        file_pths$ccei_pth,
        file_pths$map_norm_pth,
        file_pths$mwmt_norm_pth,
        file_pths$mcmt_norm_pth,
        file_pths$assess_pth,
        out_folder = out_dir(),
        overwrite = input$allow_over,
        scenario_name = input$clim_scn_nm,
        brks_mat = brks$brks_mat,
        brks_cmd = brks$brks_cmd,
        brks_ccei = brks$brks_ccei
      )

      clim_readme <- tibble(Scenario_Name = input$clim_scn_nm,
                            GCM_or_Ensemble_name = input$clim_gcm,
                            Historical_normal_period = input$clim_norm_period,
                            Future_period = input$clim_fut_period,
                            Emissions_scenario = input$clim_em_scenario,
                            Link_to_source = input$clim_dat_url,
                            brks_mat = brks_out$brks_mat %>% brks_to_txt(),
                            brks_cmd = brks_out$brks_cmd %>% brks_to_txt(),
                            brks_ccei = brks_out$brks_ccei %>% brks_to_txt())

      if(file.exists(fs::path(out_dir, "climate_data_readme.csv"))){
        clim_readme_cur <- utils::read.csv(fs::path(out_dir, "climate_data_readme.csv")) %>%
          mutate(across(everything(), as.character))

        clim_readme <- bind_rows(clim_readme_cur, clim_readme) %>%
          distinct(.data$Scenario_Name, .keep_all = TRUE)

        # set lower and upper bounds based on min and max across all scenarios
        clim_readme <- clim_readme %>%
          mutate(across(contains("brks_") & where(~!all(is.na(.x)|.x == "")), \(b){
            list(b %>% unique() %>% stringr::str_split(";") %>% unlist() %>%
                   as_tibble() %>%
                   tidyr::separate(.data$value, into = c("class", "min", "max"),
                                   sep = ": | - ", ) %>%
                   mutate(across(everything(),
                                 \(x) stringr::str_remove(x, "\\(|\\)") %>%
                                   as.numeric())) %>%
                   group_by(class) %>%
                   summarise(min = min(min), max = max(max)) %>%
                   select(min, max, class) %>% as.matrix() %>% brks_to_txt())
          }))
      }

      write.csv(clim_readme, fs::path(out_dir, "climate_data_readme.csv"),
                row.names = FALSE)

      return(brks_out)
    })

    done <- reactive({
      if(is.list(prep_done())){
        brks(prep_done())
        "Processing Complete"
      }
    })

    output$data_prep_msg <- renderText(done())

    observeEvent(input$data_done, {
      stopApp()
    })

  })
}


ui_scn <- function(id, ui_id) {
  ns <- NS(id)

  tabPanel(
    title = paste0("Scenario ", ui_id),
    textInput(ns(paste0("clim_scn_nm", ui_id)),
              labelMandatory(paste0("Scenario ", ui_id, " Name"))),
    textInput(ns(paste0("clim_scn_gcm", ui_id)), labelMandatory("GCM or Ensemble Name")),
    textInput(ns(paste0("clim_scn_period", ui_id)), labelMandatory("Future period")),
    textInput(ns(paste0("clim_scn_em", ui_id)), labelMandatory("Emissions scenario")),
    textInput(ns(paste0("clim_scn_url", ui_id)), labelMandatory("Link to Source")),

    get_file_ui2(
      id, paste0("mat_fut_pth", ui_id), mandatory = TRUE,
      title = paste0("Future mean annual temperature (Scenario ", ui_id, ")")),
    get_file_ui2(
      id, paste0("cmd_fut_pth", ui_id), mandatory = TRUE,
      title = paste0("Future climatic mositure deficit (Scenario ", ui_id, ")"))
  )
}
