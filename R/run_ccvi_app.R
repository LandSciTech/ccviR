#' Launch the CCVI app
#'
#' Launch the ccviR app to calculate the NatureServe Climate Change
#' Vulnerability Index. See \code{vignette("app_vignette", package = "ccviR")}
#' for details on how to use the app.
#'
#' @param file_dir The directory to locate files from or "demo" to use the demo
#'   data included in the package.
#' @param launch.browser logical. Run CCVI app in browser?
#' @param port If launch.browser is FALSE, specify port to run CCVI app.
#' @param test.mode Should the app be launched using shiny test.mode. Only set
#'   to TRUE for debugging.
#'
#' @import shiny
#' @import dplyr
#' @import sf
#' @import shinyFiles
#' @importFrom tmap tmap_leaflet
#' @importFrom terra has.RGB
#'
#' @export
#'
#' @returns A shiny app.
#'
#' @examples
#' if(interactive()){
#'  run_ccvi_app("demo")
#' }
run_ccvi_app <- function(file_dir = getwd(),
                         launch.browser = TRUE,
                         port = getOption("shiny.port"),
                         test.mode = FALSE){
  testmode_in <- options(shiny.testmode = test.mode)

  if(file_dir == "demo"){
    file_dir <- system.file("extdata", package = "ccviR")
  }

  shiny::shinyOptions(file_dir = file_dir)

  ccvi_app(testmode_in = testmode_in,
           launch.browser = launch.browser,
           port = port)

}
