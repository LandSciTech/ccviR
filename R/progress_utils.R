inform_prog <- function(detail, quiet, n = 100, set = FALSE) {
  if(quiet) return(invisible())

  if(shiny::isRunning()) {
    # Sys.sleep(1) # Uncomment for testing progress messages interactively
    if(!set) incProgress(1/(n + 1), detail = detail)
    if(set) setProgress(n, detail = detail)
  } else {
    message(detail)
  }
}
