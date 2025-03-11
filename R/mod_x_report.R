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
    radioButtons(ns("include_about"), label = "Include Interpretation Guide",
                 choices = c("Yes" = TRUE, "No" = FALSE), inline = TRUE),
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
      filename = {
        tolower(saved()$common_name[1]) %>%
          stringr::str_extract_all("\\w+", simplify = TRUE) %>%
          paste0(collapse = "_") %>%
          paste0("_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        req(input$include_about)
        check_chrome()
        withProgress(message = 'Creating report...', {
          build_report(saved(), file, include_about = input$include_about)
        })
      }
    )
  })

}
