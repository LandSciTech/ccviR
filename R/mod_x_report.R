#' Test the Results module
#'
#' @noRd
#' @examples
#' mod_report_test()
#' mod_report_test(saved = NULL)
#'
#' # mod_report_test(test_df_loaded("spatial")) # Test a specific saved output in extdata/test_files
#'
#' # Test if we don't have chrome/etc. installed
#' withr::with_options(
#'   list("ccviR.test_no_chrome_platform" = TRUE), {
#'     runApp(mod_report_test())
#'   })
#'
#' withr::with_options(
#'   list("ccviR.test_no_chrome" = TRUE), {
#'     runApp(mod_report_test())
#'   })
#'

mod_report_test <- function(saved = test_df_loaded()) {

  ui <- ui_setup(
    tabPanel("Index Results",
             fluidPage(div(mod_report_ui(id = "test")))))
  server <- function(input, output, session) {
    shinyOptions("file_dir" = "inst/extdata/")

    mod_report_server(id = "test", reactive(saved))
  }

  shinyApp(ui, server)
}

mod_report_ui <- function(id) {

  ns <- NS(id)

  div(
    br(), br(),

    radioButtons(ns("include_about"), label = "Report: Include Interpretation Guide",
                 choices = c("Yes" = TRUE, "No" = FALSE), inline = TRUE),
    div(style = "display:flex; align-items: center",
      div(style = "flex: 1",
          downloadButton(ns("report"), "Generate report", class = "btn-primary")),
      uiOutput(ns("validate"), class = "button-status g-col-10",
               style = "flex: 12")
    )
  )
}

mod_report_server <- function(id, saved) {

  stopifnot(is.reactive(saved))

  moduleServer(id, function(input, output, session) {

    # Check if download valid
    observe({
      shinyjs::toggleState(
        id = "report",
        condition = have_chrome() == TRUE & isTruthy(input$include_about))
    })

    output$validate <- renderUI({
      req(saved())
      check_chrome()
    })

    # Report download -------------------------------------------------------

    # NOTE: downloadHandler will always try to download, even if things error,
    #  so disable the button until we're sure it will work.
    output$report <- downloadHandler(
      filename = {
        tolower(saved()$common_name[1]) %>%
          stringr::str_extract_all("\\w+", simplify = TRUE) %>%
          paste0(collapse = "_") %>%
          paste0("_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        withProgress(message = 'Creating report...', {
          build_report(saved(), file, include_about = input$include_about)
        })
      }
    )
  })

}
