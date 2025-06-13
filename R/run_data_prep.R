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
#'  run_data_prep("demo")

run_data_prep <- function(file_dir = getwd(),
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
