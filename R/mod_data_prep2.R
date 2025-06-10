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
    file_dir <- fs::path_package("extdata", package = "ccviR")
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

mod_data_prep_test <- function(input_files = test_files_prep()) {
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
    includeMarkdown(fs::path_package("ccviR", "shiny", "00_data_prep.md")),


    h3("Setup"),

    get_file_ui2(id, "out_folder", "Output folder", mandatory = TRUE,
                 type = "dir"),
    checkboxInput(ns("allow_over"), "Overwrite existing files?"),

    h3("Historical data"),

    textInput(ns("clim_norm_period"), labelMandatory("Historical normal period")),
    textInput(ns("clim_norm_url"), labelMandatory("Link to Source")),

    get_file_ui2(id, "mat_norm_pth", "Historical mean annual temperature (MAT)", TRUE),
    get_file_ui2(id, "cmd_norm_pth", "Historical climatic mositure deficit (CMD)", TRUE),
    get_file_ui2(id, "map_norm_pth", "Historical mean annual precipitation (MAP)"),
    get_file_ui2(id, "mwmt_norm_pth", "Historical mean warmest month temperature (MWMT)"),
    get_file_ui2(id, "mcmt_norm_pth", "Historical mean coldest month temperature (MCMT)"),

    h3("Supporting data"),
    # TODO: Assessment area?
    get_file_ui2(id, "assess_pth", "Assessment area (Climate data extent polygon)"),

    h3("Future scenario data"),
    numericInput(ns("scn_n"), label = "How many future scenarios?",
                 value = 1, min = 1, max = 5),
    uiOutput(ns("ui_scenarios")),

    br(),
    div(style = "display:inline-block",
        shinyjs::disabled(
          actionButton(ns("submit"), "Process", class = "btn-primary")),
        uiOutput(ns("submit_error"), class = "button-status"),
        uiOutput(ns("prep_data_done"), class = "button-status")
    ),
    br(),
    br(),
    actionButton(ns("data_done"), "Close", class = "btn-primary")
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
    out_folder <- reactiveVal()
    file_ids <- c(
      # Historical Data files
      "mat_norm_pth", "cmd_norm_pth", "map_norm_pth",
      "mwmt_norm_pth", "mcmt_norm_pth",
      # Supporting Data files
      "assess_pth")

    # Set inputs/paths for testing ----------------------
    # Only at the start - set dir
    observeEvent(input_files, {
      if(!is.null(input_files)) {
        req(input$scn_n)
        # Set paths
        if(!is.null(getOption("ccviR.test_data_prep"))) {
          # Used in shinytest2/testthat testing only
          # So we can find the folder and test that it prepared the data
          d <- getOption("ccviR.test_data_prep")
        } else d <- "TESTING DATA UI OUTPUTS"
        out_folder(d)
        fs::dir_create(out_folder())
      }
    })

    # Any time - set file paths
    observe({
      if(!is.null(input_files)) {
        req(input$scn_n)

        # Set Inputs
        stringr::str_subset(names(input_files), "pth", negate = TRUE) %>%
          purrr::walk(~{
            updateTextInput(session, .x, value = input_files[[.x]])
          })
        updateCheckboxInput(session, "allow_over", value = TRUE)

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
      filetypes = ccviR::spatial_file_types)
    observeEvent(file_scn_ids(), {
      purrr::map(
        file_scn_ids(), shinyFileChoose, root = volumes, input = input,
        filetypes = ccviR::spatial_file_types)
    })

    # Scenarios -----------------------------------
    output$ui_scenarios <- renderUI({

      # Get file name ids for each scenario
      file_scn_ids(c(paste0("mat_fut_pth", seq_len(input$scn_n)),
                     paste0("cmd_fut_pth", seq_len(input$scn_n)),
                     paste0("ccei_pth", seq_len(input$scn_n))))

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
              file_pths[[.x]] <- stats::setNames(pth_in, nm = .x)
            }
          }, ignoreInit = TRUE)
        )
      })


    # Parse Dir paths
    observeEvent(input$out_folder, {
      if(!is.integer(input$out_folder)) {
        out_folder(parseDirPath(volumes, input$out_folder))
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
      out_folder(NULL)
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
      out_folder()
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

      scn <- paste0(c("clim_scn_nm", "clim_scn_gcm", "clim_scn_period",
                      "clim_scn_em", "clim_scn_url"), rep(sort(input$scn_n), 5))
      req_scn <- purrr::map(stats::setNames(nm = scn), ~input[[.x]])

      req_dir <- c("out_folder" = out_folder())

      c(req_pths, req_static, req_dir, req_scn)
    })

    # NOTE:
    # Could create error text boxes for all file inputs (cf mod_spatial),
    # but don't, as this is wordy and redundant, use the submit_error
    # validate(need()) instead (below).

    # Toggle button ---------------------------
    # Enable the Submit button when all mandatory fields are filled out
    output$submit_error <- renderText({

      shinyjs::disable("submit")

      have_inputs <- purrr::map_lgl(required_inputs(), ~!is.null(.x) && .x != "")
      validate(need(all(have_inputs), "Missing required inputs"))

      # Check that have both MWMT and MCMT if either provided
      validate(need(
        isTruthy(file_pths$mwmt_norm_pth) == isTruthy(file_pths$mcmt_norm_pth),
        "Must provide both MCMT and MWMT or neither"))

      shinyjs::enable("submit")
    })


    # Prepare Climate Data --------------------------

    prep_values <- reactive({

      list("prep_data" =
             list("mat_norm" = file_pths$mat_norm_pth,
                  "mat_fut" = collect_inputs(file_pths, "mat_fut_pth"),
                  "cmd_norm" = file_pths$cmd_norm_pth,
                  "cmd_fut" = collect_inputs(file_pths, "cmd_fut_pth"),
                  "ccei" = collect_inputs(file_pths, "ccei_pth"),
                  "map" = file_pths$map_norm_pth,
                  "mwmt" = file_pths$mwmt_norm_pth,
                  "mcmt" = file_pths$mcmt_norm_pth,
                  "clim_poly" = file_pths$assess_pth,
                  "in_folder" = NULL,
                  "out_folder" = out_folder(),
                  "reproject" = FALSE,
                  "overwrite" = input$allow_over,
                  "scenario_name" = collect_inputs(input, "clim_scn_nm")
             ),
           "prep_readme" = list(
             "scenario_name" = collect_inputs(input, "clim_scn_nm"),
             "gcm_ensemble" = collect_inputs(input, "clim_scn_gcm"),
             "hist_period" = input$clim_norm_period,
             "fut_period" = collect_inputs(input, "clim_scn_period"),
             "emissions_scenario" = collect_inputs(input, "clim_scn_em"),
             "url" = collect_inputs(input, "clim_scn_url"),
             "out_folder" = out_folder()
           )
      )
    })

    prep_values_last_run <- eventReactive(input$submit, prep_values())

    prep_data_done <- eventReactive(input$submit, {

      #brks <- list(brks_mat = NULL, brks_cmd = NULL, brks_ccei = NULL)

      withProgress(
        message = paste("Processing data for", input$scn_n, "scenario(s)"), {
          out <- tryCatch({
            brks <- do.call(prep_clim_data_multi, prep_values()$prep_data)
            do.call(prep_clim_readme, append(prep_values()$prep_readme, brks))
            # Testing messages:
            # stop("Example error message")  # Uncomment when interactive testing
            return(Sys.time())
          },
          error = function(cnd) {
            if(stringr::str_detect(conditionMessage(cnd), "file exists")) {
              paste0("Files exist: Check 'Overwrite existing files?' ",
                     "under 'Setup' to overwrite these files")
            } else {
              conditionMessage(cnd)
            }
          }
          )
        })
    })

    # Signal finish --------------------
    output$prep_data_done <- renderUI({ # Use renderUI when rendering HTML
      req(prep_data_done())


      if(!inherits(prep_data_done(), "POSIXct")) {
        # If error in run
        tagList(icon("xmark", style = "color:red"),
                span(strong("Error preparing data: "), prep_data_done()))
      } else {

        # When testing use placeholder
        if(is_testing()) dt <- "HH:MM PM" else dt <- format(prep_data_done(), "%I:%M %p")

        if(!list_equal(prep_values(), prep_values_last_run())) {
          # If last completed no longer matches the selected values
          tagList(icon("check", style = "color:grey"),
                  span("Last completed at", dt, style = "color:grey"))
        } else {
          # Last completed matches current values
          tagList(icon("check", style = "color:green"), "Completed at", dt)
        }
      }

    })

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
    em("This will be used as a suffix to the saved output data and to identify the scenario"),
    textInput(ns(paste0("clim_scn_gcm", ui_id)), labelMandatory("GCM or Ensemble Name")),
    textInput(ns(paste0("clim_scn_period", ui_id)), labelMandatory("Future period")),
    textInput(ns(paste0("clim_scn_em", ui_id)), labelMandatory("Emissions scenario")),
    textInput(ns(paste0("clim_scn_url", ui_id)), labelMandatory("Link to Source")),

    get_file_ui2(
      id, paste0("mat_fut_pth", ui_id), mandatory = TRUE,
      title = paste0("Future mean annual temperature (MAT; Scenario ", ui_id, ")")),
    get_file_ui2(
      id, paste0("cmd_fut_pth", ui_id), mandatory = TRUE,
      title = paste0("Future climatic mositure deficit (CMD; Scenario ", ui_id, ")")),
    get_file_ui2(id, paste0("ccei_pth", ui_id), "Climate change exposure index"),
  )
}

collect_inputs <- function(input, name) {
  purrr::map_chr(stringr::str_subset(names(isolate(input)), name), ~input[[.x]])
}

list_equal <- function(l1, l2) {
  if(is.logical(all.equal(l1, l2))) return(TRUE) else return(FALSE)
}
