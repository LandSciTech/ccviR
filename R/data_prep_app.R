


#' Title
#'
#' @param file_dir
#' @param launch.browser
#' @param port
#' @param ...
#'
#' @return
#'
#' @import shiny
#'
#' @export
#'
#' @examples
data_prep_app <- function(file_dir = getwd(),
                          launch.browser = TRUE,
                          port = getOption("shiny.port"), ...){
  # Setup #=====================================================================
  if(file_dir == "demo"){
    file_dir <- system.file("extdata", package = "ccviR")
  }

  shiny::shinyOptions(file_dir = file_dir, launch.browser = launch.browser,
                      port = port)

  # CSS to use in the app
  appCSS <-
    ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "
  # File path ids to use with file choose
  filePathIds <- c("mat_norm_pth", "mat_fut_pth", "cmd_norm_pth", "cmd_fut_pth",
                   "ccei_pth", "map_pth", "mwmt_pth", "mcmt_pth")

  # UI #========================================================================
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),

    h1("Prepare data for the CCVI App"),

    h2("Climate Data"),

    p("Cliamte data can be added by selecting file paths for each file",
      " or selecting a folder that contains all the files with standard names"),

    selectInput("data_as", "Provide data as:",
                  list(`Individual file paths` = "paths",
                       `One folder` = "folder"),
                  selected = "paths"),
    shinyjs::hidden(
      div(
        id = "folder_input",
        labelMandatory(strong("Folder location of climate data:")),
        shinyFiles::shinyDirButton("clim_var_dir", "Choose a folder",
                       "Folder location of climate data"),
        verbatimTextOutput("clim_var_dir", placeholder = TRUE),
        br(),
        strong("File names in folder"),
        tags$ul(
          tags$li(labelMandatory("MAT.asc: mean annual temperature for the historical normal period")),
          tags$li(labelMandatory("MAT_2050.asc: mean annual temperature for the future under climate change. It can be any number eg 2050, 2100")),
          tags$li(labelMandatory("CMD.asc: climate moisture deficit for the historical normal period")),
          tags$li(labelMandatory("CMD_2050.asc: climate moisture deficit for the future under climate change it can be any number eg 2050, 2100")),
          tags$li("CCEI.img: Climate Change Exposure Index from NatureServe website"),
          tags$li("MAP.asc: mean annual precipitation for the historical normal period"),
          tags$li("MWMT.asc: mean warmest month temperature for the historical normal period"),
          tags$li("MCMT.asc: mean coldest month temperature for the historical normal period")

        )
      )
    ),
    div(
      id = "paths_input",
      get_file_ui("mat_norm_pth", "Historical mean annual temperature", TRUE),
      get_file_ui("mat_fut_pth", "Future mean annual temperature", TRUE),
      get_file_ui("cmd_norm_pth", "Historical climatic mositure deficit", TRUE),
      get_file_ui("cmd_fut_pth", "Future climatic mositure deficit", TRUE),
      get_file_ui("ccei_pth", "Climate change exposure index"),
      get_file_ui("map_pth", "Historical mean annual precipitation"),
      get_file_ui("mwmt_pth", "Mean warmest month temperature"),
      get_file_ui("mcmt_pth", "Mean coldest month temperature")

    ),
    div(
      id = "folder_output",
      labelMandatory(strong("Output folder location")),
      shinyFiles::shinyDirButton("out_folder", "Choose a folder",
                                 "Output folder location"),
      verbatimTextOutput("out_folder", placeholder = TRUE),
      br()
    ),

    checkboxInput("allow_over",
                  "Should existing files in the output folder be overwritten?"),

    checkboxInput("reproj", "Should the outputs be reprojected to WGS84?"),

    actionButton("submit", "Submit", class = "btn-primary"),

    shinycssloaders::withSpinner(verbatimTextOutput("done"))


  )

  # Server #====================================================================
  server <- function(input, output, session) {
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

          out_folder = shinyFiles::parseDirPath(volumes,
                                                input$out_folder),
          reproject = input$reproj,
          overwrite = input$allow_over
        )
      }
    })

    output$done <- renderText({
      req(prep_done())
      "Processing Complete"
    })



  }

  shinyApp(ui, server,
           options = list(launch.browser = getShinyOption("launch.browser"),
                          port = getShinyOption("port")))
}
