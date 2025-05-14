# Using template: https://github.com/quarto-dev/quarto-cli/discussions/5373#discussioncomment-5772210
# Quarto Parameters: https://quarto.org/docs/computations/parameters.html
# quarto R package: https://quarto-dev.github.io/quarto-r/

#' Build the report from saved data
#'
#' @param saved Data frame. Data saved from the Shiny app.
#' @param file_loc Character. Directory to save file, or if a file name,
#'  name of file to create.
#' @param include_about Logical. Whether to include page explaining report
#'   interpretation.
#' @param overwrite Logical. Whether to overwrite previous reports with the same
#'   name.
#' @param quiet Logical. Whether to suppress progress messages.
#'
#' @returns File location for the pdf report
#' @export
#'
#' @examples
#' build_report(test_df_loaded())
#' build_report(test_df_loaded(), debug = TRUE)

build_report <- function(saved, file_loc = ".", include_about = TRUE,
                         overwrite = TRUE, quiet = FALSE, debug = FALSE) {

  inform_prog("Preparing report template", quiet, 4)

  # If we're building directly with only a location, no file path
  if(fs::path_ext(file_loc) != "pdf") {
    nm <- tolower(saved$common_name[1]) %>%
      stringr::str_extract_all("\\w+", simplify = TRUE) %>%
      paste0(collapse = "_")
    out <- fs::path(file_loc, paste0("report_", nm, "_", Sys.Date(), ".pdf"))

  # Otherwise if we're given a full file path
  } else out <- file_loc

  qmd_dir <- fs::path_package("qmd", package = "ccviR")
  t <- fs::dir_copy(qmd_dir, fs::path_temp("qmd"), overwrite = TRUE)

  if(debug) cat(t)

  # Create HTML report
  inform_prog("Rendering HTML report", quiet, 4)

  # Force all NAs to ".na" before sending
  # Otherwise:
  #  - In future my error: https://github.com/quarto-dev/quarto-r/issues/168#issuecomment-2024805212
  #  - If we let quarto_render() convert, they use more than one 'na' type
  #     which is harder to recover from (.na.real .na.character, etc.)
  saved <- as.list(saved) %>%
    purrr::map(~if_else(is.na(.x), ".na", as.character(.x)))

  quarto::quarto_render(
    fs::path(t, "results_report.qmd"),
    execute_params = list(
      saved = saved,
      # Pass these in as separate variables so they can be used in the header
      species = saved$common_name[1],
      assessor_name = saved$assessor_name[1],
      geo_location = saved$geo_location[1],
      include_about = include_about,
      debug = debug
    ),
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


check_chrome <- function() {

  chrome <- have_chrome()

  if(chrome != TRUE) {
    if(stringr::str_detect(chrome, "Cannot find")) {
      msg <- paste0("Chrome, Chromium, or Edge is required to build reports ",
                    "but cannot be found. Do you need to install it?")
    } else if (stringr::str_detect(chrome, "platform is not supported")) {
      msg <- "Unfortunately your platform is not supported for generating reports"
    }

    validate(need(FALSE, msg))
  }
}



#' Test whether Chrome is installed
#'
#' This is a wrapper around `pagedown::find_chrome()` for several reasons.
#'
#' 1. It turns errors into messages which we can process
#' 2. Allows us to 'mock' this in our testthat tests without modifying
#'  `pagedown::find_chrome()` directly (safer:
#'  https://testthat.r-lib.org/reference/local_mocked_bindings.html#namespaced-calls).
#'  BUT NOTE: We can't currently mock shinytest2 tests
#'  (https://github.com/rstudio/shinytest2/issues/371), so we artificially mock
#'  with options.#'
#'
#' @returns TRUE or the error message from `pagedown::find_chrome()` as a string
#' @noRd
#'
#' @examples
#' have_chrome()
have_chrome <- function() {

  # For testing only, these messages come from `pagedown::find_chrome`
  if(shiny::isTruthy(getOption("ccviR.test_no_chrome"))) {
    return("Cannot find Chromium or Google Chrome")
  } else if(shiny::isTruthy(getOption("ccviR.test_no_chrome_platform"))) {
    return("Your platform is not supported")
  }

  tryCatch({
    is.character(pagedown::find_chrome())
  }, error = function(e) e$message
  )
}

