# Data prep module

# UI #========================================================================
  data_prep_ui <- function(id){
    fluidPage(
      # shinyjs::useShinyjs(),
      # shinyjs::inlineCSS(appCSS),
      h2("Prepare data for the CCVI App"),
      p(strong("Step 1: "), "Download climate data from ",
        a("AdaptWest.", href = "https://adaptwest.databasin.org/pages/adaptwest-climatena/"),
        "Select the bioclimatic variables for the normal period and the desired future climate scenario. ",
        "We recommend using the 1961-1990 normal period and ",
        "the ensemble data for SSP2-4.5 2050s for the future. ",
        "Save the downloaded data in a folder you can easily find."),

      p(strong("Step 2: "), "Prepare the climate data for use in the app.",
        "Climate data can be added by selecting file paths for each file",
        " or selecting a folder that contains all the files with standard names.",
        " For the output folder make sure to choose a location that is easy to find again",
        " because you will use the prepared climate data to calculate the index."),

      selectInput(NS(id, "data_as"), "Provide data as:",
                  list(`Individual file paths` = "paths",
                       `One folder` = "folder"),
                  selected = "folder"),
      shinyjs::hidden(
        div(
          id = NS(id, "folder_input"),
          labelMandatory(strong("Folder location of raw climate data:")),
          shinyFiles::shinyDirButton(NS(id, "clim_var_dir"), "Choose a folder",
                                     "Folder location of raw climate data"),
          verbatimTextOutput(NS(id, "clim_var_dir"), placeholder = TRUE),
          br(),
          strong("File names in folder"),
          tags$ul(
            tags$li(labelMandatory("MAT: mean annual temperature for the historical normal period")),
            tags$li(labelMandatory("MAT_2050: mean annual temperature for the future under climate change. It can be any number eg 2050, 2100")),
            tags$li(labelMandatory("CMD: climate moisture deficit for the historical normal period")),
            tags$li(labelMandatory("CMD_2050: climate moisture deficit for the future under climate change it can be any number eg 2050, 2100")),
            tags$li("CCEI: Climate Change Exposure Index from NatureServe website"),
            tags$li("MAP: mean annual precipitation for the historical normal period"),
            tags$li("MWMT: mean warmest month temperature for the historical normal period"),
            tags$li("MCMT: mean coldest month temperature for the historical normal period")

          ),
          p('Accepted filetypes are ".asc", ".tif", ".nc", ".grd" and ".img"'),
          tags$ul(tags$li("clim_poly: An optional shapefile with a polygon of the extent of the climate data. It will be created from the climate data if it is missing but it is faster to provide it."))
        )
      ),
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

      checkboxInput(NS(id, "reproj"), "Should the outputs be reprojected to WGS84?"),

      actionButton(NS(id, "submit"), "Process", class = "btn-primary"),
    )
  }

  # Server #====================================================================
  data_prep_server <- function(id){
    moduleServer(id, function(input, output, session) {
    observe({
      if(input$data_as == "folder"){
        shinyjs::hide("paths_input")
        shinyjs::show("folder_input")
      }
      if(input$data_as == "paths"){
        shinyjs::hide("folder_input")
        shinyjs::show("paths_input")
      }
    })
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
               filetypes = c("shp", "tif", "asc", "nc", "grd", "bil"))

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

    prep_done <- eventReactive(input$submit, {
      if(isTruthy(input$clim_var_dir)){
        run_prep_data(in_folder = shinyFiles::parseDirPath(volumes,
                                                           input$clim_var_dir),
                      out_folder = shinyFiles::parseDirPath(volumes,
                                                            input$out_folder),
                      reproject = input$reproj,
                      overwrite = input$allow_over)
      } else {
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

          out_folder = shinyFiles::parseDirPath(volumes,
                                                input$out_folder),
          reproject = input$reproj,
          overwrite = input$allow_over
        )
      }
    })

    reactive({
      if(prep_done() == ""){
        "Processing Complete"
      }
    })



  })
  }

#   shinyApp(ui, server,
#            options = list(launch.browser = getShinyOption("launch.browser"),
#                           port = getShinyOption("port")))
# }
