# Using template: https://github.com/quarto-dev/quarto-cli/discussions/5373#discussioncomment-5772210
# Quarto Parameters: https://quarto.org/docs/computations/parameters.html
# quarto R package: https://quarto-dev.github.io/quarto-r/

#' Build the report from saved data
#'
#' @param saved Data frame. Data saved from the Shiny app.
#' @param file_loc Character. Directory to save file, or if a file name,
#'  name of file to create.
#' @param overwrite Logical. Whether to overwrite previous reports with the same
#'   name.
#' @param quiet Logical. Whether to suppress progress messages.
#'
#' @returns File location for the pdf report
#' @export
#'
#' @examples
#' build_report(test_df_loaded())

build_report <- function(saved, file_loc = ".", overwrite = TRUE, quiet = FALSE) {


  inform_prog("Preparing report template", quiet, 4)

  if(fs::is_file(file_loc)) {
    nm <- tolower(saved$common_name[1]) |>
      stringr::str_extract_all("\\w+", simplify = TRUE) |>
      paste0(collapse = "_")
    out <- fs::path(file_loc, paste0("report_", nm, "_", Sys.Date(), ".pdf"))
  } else out <- file_loc

  qmd_dir <- fs::path_package("qmd", package = "ccviR")
  t <- fs::dir_copy(qmd_dir, fs::path_temp("qmd"), overwrite = TRUE)

  # Create HTML report
  inform_prog("Rendering HTML report", quiet, 4)

  quarto::quarto_render(
    fs::path(t, "results_report.qmd"),
    execute_params = list(saved = saved,
                          species = saved$common_name[1])
    )

  # Print to PDF via Chrome
  inform_prog("Converting to PDF", quiet, 4)
  pagedown::chrome_print(fs::path(t, "results_report.html"))

  # Move pdf

  fs::file_copy(fs::path(t, "results_report.pdf"), out, overwrite = overwrite)

  # Clean up
  fs::dir_delete(t)
  inform_prog("Done!", quiet, 4)
  # Return link to file
  out
}
