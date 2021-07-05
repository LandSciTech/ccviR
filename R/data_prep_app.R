
data_prep_app <- function(file_dir = getwd(),
                          launch.browser = TRUE,
                          port = getOption("shiny.port"), ...){

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


  ui <- fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),

    h1("Prepare data for the CCVI App"),

    h2("Climate Data"),

    p("Cliamte data can be added by selecting file paths for each file",
      " or selecting a folder that contains all the files with standard names"),

    checkboxGroupInput("data_as", "Provide data as:",
                  list(`Individual file paths` = "paths",
                       `One folder` = "folder"),
                  selected = "paths",
                  inline = T),
    shinyjs::hidden(
      div(
        id = "folder_input",
        labelMandatory(strong("Folder location of climate data:")),
        shinyDirButton("clim_var_dir", "Choose a folder",
                       "Folder location of climate data"),
        verbatimTextOutput("clim_var_dir", placeholder = TRUE),
        br()
      )
    ),
    div(
      id = "paths_input",
      get_file_ui("mat_norm_pth", "Historical mean annual temperature")
    ),

  )

  server <- function(input, output, session) {

  }

  shinyApp(ui, server,
           options = list(launch.browser = getShinyOption("launch.browser"),
                          port = getShinyOption("port")))
}
