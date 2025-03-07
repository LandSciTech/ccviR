inform_prog <- function(detail, quiet, n = NULL) {
  if(quiet) return(invisible())

  if(shiny::isRunning()) {
    incProgress(1/(n + 1), detail = detail)
  } else {
    message(detail)
  }
}
