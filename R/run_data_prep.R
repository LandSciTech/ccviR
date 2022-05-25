#' Launch the data preparation app
#'
#' Launch the data preparation app for the ccviR package. See
#' \code{vignette("data_prep_vignette", package = "ccviR")} for details on how to use
#' the app.
#'
#' @param file_dir The directory to locate files from
#' @param launch.browser logical. Run app in browser?
#' @param port If launch.browser is FALSE, specify port to run CCVI app
#' @param test.mode Should the app be launched using shiny test.mode. Only set
#'   to TRUE for debugging.
#'
#' @export
#'
#' @returns A shiny app.
#'
#' @examples
#' if(interactive()){
#'  run_data_prep("demo")
#' }
run_data_prep <- function(file_dir = getwd(),
                          launch.browser = TRUE,
                          port = getOption("shiny.port"),
                          test.mode = FALSE){
  testmode_in <- options(shiny.testmode = test.mode)

  if(file_dir == "demo"){
    file_dir <- system.file("extdata", package = "ccviR")
  }

  shiny::shinyOptions(file_dir = file_dir)

  data_prep_app(testmode_in = testmode_in,
                launch.browser = launch.browser,
                port = port)

}
