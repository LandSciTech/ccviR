inform_prog <- function(detail, quiet, n = NULL, i = NULL) {
  if(quiet) return(invisible())

  if(shiny::isRunning()) {
    incProgress(i/n, detail = detail)
  } else {
    message(detail)
  }
}
