# Data prep module

# UI #========================================================================

data_prep_ui <- function(id){
    fluidPage(
      h2("Prepare data for the CCVI App"),
      p(em("NOTE: Running this app is only necessary if you want to run the ",
           "CCVI app with custom climate data. To use the pre-prepared climate ",
           "data download a zip file from ",
           a("here", href = "https://drive.google.com/drive/folders/18mO5GDUmwi-nswhIAC36bmtYsvmqNQkH?usp=share_link", target="_blank"),
           ", unzip it and save the folder in a convenient location.")),
      p(strong("Step 1: "), "Acquire climate data including: ",
        "Mean annual temperature (MAT), climate moisture deficeit (CMD), ",
        "mean annual precipitation (MAP), and minimum coldest and ",
        "maximum warmest month temperatures (MCMT, MWMT) for a historical period",
        "and MAT and CMD for a future period. ",
        "To calculate a migratory exposure index acquire a climate change ",
        "exposure index (CCEI) raster for the migratory region. For example, ",
        "the CCEI for South America is available from",
        a("NatureServe.", href = "https://www.natureserve.org/ccvi-species"),
        "To download climate data from ",
        a("AdaptWest.", href = "https://adaptwest.databasin.org/pages/adaptwest-climatena/"),
        "Select the bioclimatic variables for the normal period and the desired future climate scenario(s). ",
        "We recommend using the 1961-1990 normal period and ",
        "the ensemble data for SSP1-2.6, SSP2-4.5 and SSP5-8.5 2050s for the future. ",
        "Save the downloaded data in a folder you can easily find. ",
        "When processing multiple climate scenarios the first scenario ",
        "processed will determine the thresholds used to classify the ",
        "temperature and moisture exposure based on the median and 1/2 the ",
        "interquartile range. All scenarios must be processed in one session ",
        "for the correct thresholds to be applied.",
        "Repeat steps 2 and 3 for each scenario."),

      p(strong("Step 2:"), "Record a description of the climate data. ",
        "The Sceanrio Name should be short and an appropriate title for results. ",
        "It will be used as a suffix to the saved output data and to identify the scenario."),

      textInput(NS(id, "clim_scn_nm"), labelMandatory("Scenario Name")),
      textInput(NS(id, "clim_gcm"), labelMandatory("GCM or Ensemble Name")),
      textInput(NS(id, "clim_norm_period"), labelMandatory("Historical normal period")),
      textInput(NS(id, "clim_fut_period"), labelMandatory("Future period")),
      textInput(NS(id, "clim_em_scenario"), labelMandatory("Emissions scenario")),
      textInput(NS(id, "clim_dat_url"), labelMandatory("Link to Source")),

      p(strong("Step 3: "), "Prepare the climate data for use in the app.",
        "Climate data can be added by selecting file paths for each file. ",
        "For the output folder make sure to choose a location that is easy to find again ",
        "because you will use the prepared climate data to calculate the index.",
        "The output folder should be the same for all scenarios."),

      get_file_ui(NS(id, "mat_norm_pth"), "Historical mean annual temperature", TRUE),
      get_file_ui(NS(id, "mat_fut_pth"), "Future mean annual temperature", TRUE),
      get_file_ui(NS(id, "cmd_norm_pth"), "Historical climatic mositure deficit", TRUE),
      get_file_ui(NS(id, "cmd_fut_pth"), "Future climatic mositure deficit", TRUE),
      get_file_ui(NS(id, "ccei_pth"), "Climate change exposure index"),
      get_file_ui(NS(id, "map_pth"), "Historical mean annual precipitation"),
      get_file_ui(NS(id, "mwmt_pth"), "Mean warmest month temperature"),
      get_file_ui(NS(id, "mcmt_pth"), "Mean coldest month temperature"),
      get_file_ui(NS(id, "clim_poly_pth"), "Climate data extent polygon"),
      get_file_ui(NS(id, "out_folder"), "Output folder location", mandatory = TRUE,
                  type = "dir"),

      checkboxInput(NS(id, "allow_over"),
                    "Should existing files in the output folder be overwritten?"),

      actionButton(NS(id, "submit"), "Process", class = "btn-primary")

    )
  }

  # Server #====================================================================
  data_prep_server <- function(id){
    moduleServer(id, function(input, output, session) {

    # start up Note this time out is because when I disconnected from VPN it
    # made the getVolumes function hang forever because it was looking for
    # drives that were no longer connected. Now it will give an error
    R.utils::withTimeout({
      volumes <- c(wd = getShinyOption("file_dir"),
                   Home = fs::path_home(),
                   shinyFiles::getVolumes()())
    },
    timeout = 10, onTimeout = "error")

    # File path ids to use with file choose
    filePathIds <- c("mat_norm_pth", "mat_fut_pth", "cmd_norm_pth", "cmd_fut_pth",
                     "ccei_pth", "map_pth", "mwmt_pth", "mcmt_pth", "clim_poly_pth")

    # Find file paths
    shinyFiles::shinyDirChoose(input, "out_folder", root = volumes)

    purrr::map(filePathIds, shinyFiles::shinyFileChoose, root = volumes,
               input = input,
               filetypes = c("shp", "tif", "asc", "nc", "grd", "bil", ".img"))

    file_pths <- reactive({
      purrr::map(filePathIds, ~{
        if(is.integer(input[[.x]])){
          return(NULL)
        } else {
          return(parseFilePaths(volumes, input[[.x]])$datapath)
        }

      }) %>% purrr::set_names(filePathIds)
    })

    # output file paths
    observe({
      purrr::walk2(file_pths(), filePathIds, ~{
        out_name <- paste0(.y, "_out")
        output[[out_name]] <- renderText({.x})
      })
    })

    output$out_folder_out <- renderText({
      shinyFiles::parseDirPath(volumes, input$out_folder)
    })

    # Enable the Submit button when all mandatory fields are filled out
    observe({
      fieldsMandatory1 <- c("clim_scn_nm", "clim_gcm", "clim_norm_period",
                            "clim_fut_period", "clim_em_scenario", "clim_dat_url")
      fieldsMandatory2 <- c("mat_norm_pth", "mat_fut_pth", "cmd_norm_pth",
                             "cmd_fut_pth")

      mandatoryFilled1 <-
        vapply(fieldsMandatory1,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled1 <- all(mandatoryFilled1)

      mandatoryFilled2 <-
        vapply(fieldsMandatory2,
               function(x) {
                 isTruthy(file_pths()[[x]])
               },
               logical(1))
      mandatoryFilled2 <- all(c(mandatoryFilled2,
                                isTruthy(shinyFiles::parseDirPath(volumes, input$out_folder))))

      shinyjs::toggleState(id = "submit",
                           condition = all(mandatoryFilled1, mandatoryFilled2))
    })

    brks <- reactiveVal(list(brks_mat = NULL, brks_cmd = NULL, brks_ccei = NULL))

    prep_done <- eventReactive(input$submit, {



      if (isTRUE(getOption("shiny.testmode"))) {
        out_dir <- system.file("extdata/clim_files/processed", package = "ccviR")
      } else {
        out_dir <- shinyFiles::parseDirPath(volumes,
                                            input$out_folder)

      }


      if(isTRUE(getOption("shiny.testmode"))){

        in_dir <- system.file("extdata/clim_files/raw", package = "ccviR")

        req(in_dir)
        req(out_dir)

        message("doing folder")

        brks_out <- prep_clim_data(in_folder = in_dir,
                      out_folder = out_dir,
                      reproject = FALSE,
                      overwrite = input$allow_over,
                      scenario_name = input$clim_scn_nm,
                      brks_mat = brks()$brks_mat,
                      brks_cmd = brks()$brks_cmd,
                      brks_ccei = brks()$brks_ccei)
      } else {
        message("Processing data")
        brks_out <- prep_clim_data(
          file_pths()$mat_norm_pth,
          file_pths()$mat_fut_pth,
          file_pths()$cmd_norm_pth,
          file_pths()$cmd_fut_pth,
          file_pths()$ccei_pth,
          file_pths()$map_pth,
          file_pths()$mwmt_pth,
          file_pths()$mcmt_pth,
          file_pths()$clim_poly_pth,
          out_folder = out_dir,
          overwrite = input$allow_over,
          scenario_name = input$clim_scn_nm,
          brks_mat = brks()$brks_mat,
          brks_cmd = brks()$brks_cmd,
          brks_ccei = brks()$brks_ccei
        )
      }

      # save brks from first calculation
      if(is.null(brks()$brks_mat)){
        brks_save <- brks_out
      } else {
        brks_save <- brks()
      }

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
      }

      write.csv(clim_readme, fs::path(out_dir, "climate_data_readme.csv"),
                row.names = FALSE)

      return(brks_out)
    })

    reactive({
      if(is.list(prep_done())){
        brks(prep_done())
        "Processing Complete"
      }
    })



  })
  }

  # shinyApp(data_prep_ui, data_prep_server,
  #          options = list(launch.browser = getShinyOption("launch.browser"),
  #                         port = getShinyOption("port")))
# }
