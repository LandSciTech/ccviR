# Data prep module

# UI #========================================================================
  data_prep_ui <- function(id){
    fluidPage(
      # shinyjs::useShinyjs(),
      # shinyjs::inlineCSS(appCSS),
      h2("Prepare data for the CCVI App"),
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
        "Repeat steps 2 and three for each scenario."),

      p(strong("Step 2:"), "Record a description of the climate data. ",
        "The Sceanrio Name should be short and an appropriate title for results. ",
        "It will be used as a suffix to the saved output data and to identify the scenario."),

      textInput(NS(id, "clim_scn_nm"),"Scenario Name"),
      textInput(NS(id, "clim_gcm"),"GCM or Ensemble Name"),
      textInput(NS(id, "clim_norm_period"), "Historical normal period"),
      textInput(NS(id, "clim_fut_period"), "Future period"),
      textInput(NS(id, "clim_em_scenario"), "Emissions scenario"),
      textInput(NS(id, "clim_dat_url"), "Link to Source"),

      p(strong("Step 3: "), "Prepare the climate data for use in the app.",
        "Climate data can be added by selecting file paths for each file. ",
        "For the output folder make sure to choose a location that is easy to find again ",
        "because you will use the prepared climate data to calculate the index.",
        "The output folder should be the same for all scenarios."),

      div(
        id = NS(id, "paths_input"),
        get_file_ui(NS(id, "mat_norm_pth"), "Historical mean annual temperature", TRUE),
        get_file_ui(NS(id, "mat_fut_pth"), "Future mean annual temperature", TRUE),
        get_file_ui(NS(id, "cmd_norm_pth"), "Historical climatic mositure deficit", TRUE),
        get_file_ui(NS(id, "cmd_fut_pth"), "Future climatic mositure deficit", TRUE),
        get_file_ui(NS(id, "ccei_pth"), "Climate change exposure index"),
        get_file_ui(NS(id, "map_pth"), "Historical mean annual precipitation"),
        get_file_ui(NS(id, "mwmt_pth"), "Mean warmest month temperature"),
        get_file_ui(NS(id, "mcmt_pth"), "Mean coldest month temperature"),
        get_file_ui(NS(id, "clim_poly_pth"), "Climate data extent polygon")
      ),

      div(
        id = NS(id, "folder_output"),
        labelMandatory(strong("Output folder location of prepared climate data")),
        shinyFiles::shinyDirButton(NS(id, "out_folder"), "Choose a folder",
                                   "Output folder location"),
        verbatimTextOutput(NS(id, "out_folder"), placeholder = TRUE),
        br()
      ),

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
    }, timeout = 10, onTimeout = "error")

    # File path ids to use with file choose
    filePathIds <- c("mat_norm_pth", "mat_fut_pth", "cmd_norm_pth", "cmd_fut_pth",
                     "ccei_pth", "map_pth", "mwmt_pth", "mcmt_pth", "clim_poly_pth")

    # Find file paths
    shinyFiles::shinyDirChoose(input, "clim_var_dir", root = volumes)

    shinyFiles::shinyDirChoose(input, "out_folder", root = volumes)

    purrr::map(filePathIds, shinyFiles::shinyFileChoose, root = volumes,
               input = input,
               filetypes = c("shp", "tif", "asc", "nc", "grd", "bil", ".img"))

    output$mat_norm_pth <- renderText({
      shinyFiles::parseFilePaths(volumes, input$mat_norm_pth)$datapath
    })

    output$mat_fut_pth <- renderText({
      shinyFiles::parseFilePaths(volumes, input$mat_fut_pth)$datapath
    })

    output$cmd_norm_pth <- renderText({
      shinyFiles::parseFilePaths(volumes, input$cmd_norm_pth)$datapath
    })

    output$cmd_fut_pth <- renderText({
      shinyFiles::parseFilePaths(volumes, input$cmd_fut_pth)$datapath
    })

    output$ccei_pth <- renderText({
      shinyFiles::parseFilePaths(volumes, input$ccei_pth)$datapath
    })

    output$map_pth <- renderText({
      shinyFiles::parseFilePaths(volumes, input$map_pth)$datapath
    })

    output$mwmt_pth <- renderText({
      shinyFiles::parseFilePaths(volumes, input$mwmt_pth)$datapath
    })

    output$mcmt_pth <- renderText({
      shinyFiles::parseFilePaths(volumes, input$mcmt_pth)$datapath
    })

    output$clim_poly_pth <- renderText({
      shinyFiles::parseFilePaths(volumes, input$clim_poly_pth)$datapath
    })

    output$clim_var_dir <- renderText({
      shinyFiles::parseDirPath(volumes, input$clim_var_dir)
    })

    output$out_folder <- renderText({
      shinyFiles::parseDirPath(volumes, input$out_folder)
    })

    brks <- reactiveVal(list(brks_mat = NULL, brks_cmd = NULL, brks_ccei = NULL))

    prep_done <- eventReactive(input$submit, {

      clim_readme <- tibble(Scenario_Name = input$clim_scn_nm,
                            GCM_or_Ensemble_name = input$clim_gcm,
                            Historical_normal_period = input$clim_norm_period,
                            Future_period = input$clim_fut_period,
                            Emissions_scenario = input$clim_em_scenario,
                            Link_to_source = input$clim_dat_url)

      if (isTRUE(getOption("shiny.testmode"))) {
        out_dir <- system.file("extdata/clim_files/processed", package = "ccviR")
      } else {
        out_dir <- shinyFiles::parseDirPath(volumes,
                                            input$out_folder)

      }

      if(file.exists(fs::path(out_dir, "climate_data_readme.csv"))){
        clim_readme_cur <- read.csv(fs::path(out_dir, "climate_data_readme.csv"))

        clim_readme <- bind_rows(clim_readme_cur, clim_readme) %>%
          distinct(.data$Scenario_Name, .keep_all = TRUE)
      }

      write.csv(clim_readme, fs::path(out_dir, "climate_data_readme.csv"),
                row.names = FALSE)


      if(input$data_as == "folder"||isTRUE(getOption("shiny.testmode"))){

        if (isTRUE(getOption("shiny.testmode"))) {
          in_dir <- system.file("extdata/clim_files/raw", package = "ccviR")
        } else {
          in_dir <- shinyFiles::parseDirPath(volumes,
                                             input$clim_var_dir)
        }

        req(in_dir)
        req(out_dir)

        message("doing folder")

        run_prep_data(in_folder = in_dir,
                      out_folder = out_dir,
                      reproject = FALSE,
                      overwrite = input$allow_over,
                      scenario_name = input$clim_scn_nm,
                      brks_mat = brks()$brks_mat,
                      brks_cmd = brks()$brks_cmd,
                      brks_ccei = brks()$brks_ccei)
      } else {
        message("doing indiv files")
        run_prep_data(
          shinyFiles::parseFilePaths(volumes, input$mat_norm_pth)$datapath,
          shinyFiles::parseFilePaths(volumes, input$mat_fut_pth)$datapath,
          shinyFiles::parseFilePaths(volumes, input$cmd_norm_pth)$datapath,
          shinyFiles::parseFilePaths(volumes, input$cmd_fut_pth)$datapath,
          shinyFiles::parseFilePaths(volumes, input$ccei_pth)$datapath,
          shinyFiles::parseFilePaths(volumes, input$map_pth)$datapath,
          shinyFiles::parseFilePaths(volumes, input$mwmt_pth)$datapath,
          shinyFiles::parseFilePaths(volumes, input$mcmt_pth)$datapath,
          shinyFiles::parseFilePaths(volumes, input$clim_poly_pth)$datapath,
          out_folder = out_dir,
          overwrite = input$allow_over,
          scenario_name = input$clim_scn_nm,
          brks_mat = brks()$brks_mat,
          brks_cmd = brks()$brks_cmd,
          brks_ccei = brks()$brks_ccei
        )
      }
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
