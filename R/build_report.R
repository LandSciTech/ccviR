

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
#'
#' build_report(test_df_loaded(), ".")

build_report <- function(saved, file_loc, qmd_dir = NULL) {

  if(is.null(qmd_dir)) qmd_dir <- fs::path_package("qmd", package = "ccviR")

  t <- fs::dir_copy(qmd_dir, fs::path_temp("qmd"))

  quarto::quarto_render(
    fs::path(t, "results_report.qmd"),
    execute_params = list(saved = saved))

  pagedown::chrome_print(fs::path(t, "results_report.html"))
#  file.copy(file.path(tempdir(), 'report.pdf'), file)

  fs::file_copy(fs::path(t, "results_report.pdf"), file_loc)

  # Clean up
  fs::dir_delete(t)
}
