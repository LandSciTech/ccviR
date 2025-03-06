#' Test the Results module
#'
#' @noRd
#' @examples
#' mod_report_test()
#' mod_report_test(saved = NULL)

mod_report_test <- function(saved = test_df_loaded()) {

  ui <- ui_setup(mod_report_ui(id = "test"))
  server <- function(input, output, session) {
    shinyOptions("file_dir" = "inst/extdata/")

    mod_report_server(id = "test", reactive(saved))
  }

  shinyApp(ui, server)
}

mod_report_ui <- function(id) {

  ns <- NS(id)

  div(
    id = "footer",
    style = "float:left",
    br(), br(),
    downloadButton(ns("report"), "Generate report", class = "btn-primary")
  )
}

mod_report_server <- function(id, saved) {

  stopifnot(is.reactive(saved))

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Report download -------------------------------------------------------
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "results_report.pdf",
      content = function(file) {
        withProgress(message = 'Report rendering in progress...', {
          # Copy the `qmd` folder to a temp directory before processing it, in
          # case we don't have write permissions to the current working dir
          # (which can happen when deployed).
          #t <- fs::dir_copy(fs::path_package("qmd", package = "ccviR"), fs::path_temp())
          build_report(saved(), file)
        })
      }
    )
  })

}
