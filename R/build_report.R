

# Using template: https://github.com/quarto-dev/quarto-cli/discussions/5373#discussioncomment-5772210
# Quarto Parameters: https://quarto.org/docs/computations/parameters.html
# quarto R package: https://quarto-dev.github.io/quarto-r/

#' Title
#'
#' @param index
#'
#' @returns
#' @export
#'
#' @examples
#' saved <- test_files()$saved$final2 %>%
#'   load_previous()
#'
#' build_report(saved)

build_report <- function(saved) {

  # Set up parameters to pass to Rmd document
  params <- list(saved = saved)

  # Knit the document, passing in the `params` list, and eval it in a
  # child of the global environment (this isolates the code in the document
  # from the code in this app).

  quarto::quarto_render(
    fs::path_package("qmd", "results_report.qmd", package = "ccviR"),
    execute_params = params)

  pagedown::chrome_print(fs::path_package("qmd", "results_report.html", package = "ccviR"))
#  file.copy(file.path(tempdir(), 'report.pdf'), file)

}
