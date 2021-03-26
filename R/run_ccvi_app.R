

#' Launch the CCVI app
#'
#' Run this function to launch the app.
#'
#' @param file_dir The directory to locate files from
#' @param launch.browser logical. Run CCVI app in browser?
#' @param port If launch.browser is FALSE, specify port to run CCVI app
#'
#' @export
#'
#' @examples if(interactive()){
#' run_ccvi_app()
#' }
run_ccvi_app <- function(file_dir = getwd(),
                         launch.browser = TRUE,
                         port = getOption("shiny.port")){
    shiny::shinyOptions(file_dir = file_dir, launch.browser = launch.browser,
                        port = port)
    source(system.file("shiny/app.R", package = "ccviR"), local = TRUE,
                       chdir = TRUE)$value

  #
  # app_path <- system.file("shiny", package = "ccviR")
  # return(shiny::runApp(app_path, launch.browser = launch.browser, port = port))
}
