#' Only use Shiny progress if running Shiny
#'
#' Wrapper around `withProgress()`
#'
#' @param message withProgress() message arg
#' @param expr withProgress() expr arg
#'
#' @noRd

with_progress <- function(message, expr) {
  if(shiny::isRunning()) {
    return(withProgress(message = message, expr = expr))
  } else {
    return(expr)
  }
}


#' Use progress messages
#'
#' Uses Shiny or normal progress messages depending on whether Shiny is running.
#'
#' @param detail Character. Message
#' @param quiet Logical. Suppress Messages
#' @param n Numeric. Progress to set (if `set = TRUE`) or total number of steps
#'   in the whole process.
#' @param set Logical. Whether to set the progress value to `n` or to use
#'   incremental steps.
#'
#' @noRd

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
