inform_prog <- function(detail, quiet = FALSE, n = 100, set = FALSE) {
  if(quiet) return(invisible())
  session <- getDefaultReactiveDomain()
  # If running Shiny and running a section where Shiny progess messages have been set
  if(shiny::isRunning() && session$progressStack$size() != 0) {
    # Sys.sleep(1) # Uncomment for testing progress messages interactively
    if(!set) incProgress(1/(n + 1), detail = detail)
    if(set) setProgress(n, detail = detail)
  } else {
    message(detail)
  }
}
