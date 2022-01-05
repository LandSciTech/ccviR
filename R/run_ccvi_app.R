

#' Launch the CCVI app
#'
#' Launch the ccviR app to calculate the NatureServe Climate Change
#' Vulnerability Index. See \code{vignette("app_vignette", package = "ccviR")}
#' for details on how to use the app.
#'
#' @param file_dir The directory to locate files from
#' @param launch.browser logical. Run CCVI app in browser?
#' @param port If launch.browser is FALSE, specify port to run CCVI app
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
                         port = getOption("shiny.port")){
  if(file_dir == "demo"){
    file_dir <- system.file("extdata", package = "ccviR")
  }

  shiny::shinyOptions(file_dir = file_dir, launch.browser = launch.browser,
                      port = port)

  ccvi_app()

}
