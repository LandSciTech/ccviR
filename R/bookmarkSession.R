#' Save a local bookmark of the session
#'
#' Modified from this stackoverflow answer: https://stackoverflow.com/a/68253464/3277050
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @examples
#'
#' library(shiny)
#'
#' ui <- function(request) {
#'   fluidPage(
#'     shinyjs::useShinyjs(),
#'     textInput("control_label", "This controls some of the labels:", "LABEL TEXT"),
#'     numericInput("inNumber", "Number input:", min = 1, max = 20, value = 5, step = 0.5 ),
#'     radioButtons("inRadio", "Radio buttons:", c("label 1" = "option1", "label 2" = "option2", "label 3" = "option3")),
#'     fileInput("restore_bookmark", "Restore Session", multiple = FALSE, accept = ".rds"),
#'     actionButton("save_inputs", 'Save Session', icon = icon("download"))
#'   )
#' }
#'
#'
#' server <-  function(input, output, session) {
#'
#' save_bookmark(input, output, session)
#' load_bookmark(input, output, session, "restore_bookmark")
#' }
#'
#' shinyApp(ui, server, enableBookmarking = "server")


save_bookmark <- function(input, output, session, save_id){
  latestBookmarkURL <- reactiveVal()

  onBookmarked(
    fun = function(url) {
      latestBookmarkURL(parseQueryString(url))
    }
  )

  onRestored(function(state) {
    showNotification(paste("Restored session:", basename(state$dir)), duration = 10, type = "message")
  })

  observeEvent(input[[save_id]], {
    showModal(modalDialog(
      title = "Session Name",
      textInput("session_name", "Please enter a session name (optional):"),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("download_inputs", "OK")
      )
    ))
  }, ignoreInit = TRUE)

  # SAVE SESSION
  output$download_inputs <- downloadHandler(
    filename = function() {
      removeModal()
      session$doBookmark()
      if (input$session_name != "") {

        tmp_session_name <- sub("\\.rds$", "", input$session_name)

        # "Error: Invalid state id" when using special characters - removing them:
        tmp_session_name <- stringr::str_replace_all(tmp_session_name, "[^[:alnum:]]", "")
        # TODO: check if a valid filename is provided (e.g. via library(shinyvalidate)) for better user feedback

        tmp_session_name <- paste0(tmp_session_name, ".rds")

      } else {
        paste(req(latestBookmarkURL()), "rds", sep = ".")
      }
    },
    content = function(file) {
      # copy the bookmark from where shiny put it to where the user can find it
      file.copy(from = file.path(
        ".",
        "shiny_bookmarks",
        req(latestBookmarkURL()),
        "input.rds"
      ),
      to = file)
    }
  )
}

load_bookmark <- function(input, output, session, restore_id){
  # LOAD SESSION
  observeEvent(input[[restore_id]], {

    sessionName <- fs::path_ext_remove(input[[restore_id]]$name)
    targetPath <- file.path(".", "shiny_bookmarks", sessionName, "input.rds")

    if (!dir.exists(dirname(targetPath))) {
      dir.create(dirname(targetPath), recursive = TRUE)
    }

    # copy the bookmark to where shiny expects it to be
    file.copy(
      from = input[[restore_id]]$datapath,
      to = targetPath,
      overwrite = TRUE
    )

    restoreURL <- paste0(session$clientData$url_protocol, "//",
                         session$clientData$url_hostname, ":",
                         session$clientData$url_port, "/?_state_id_=",
                         sessionName)

    # redirect user to restoreURL
    shinyjs::runjs(sprintf("window.location = '%s';", restoreURL))

    # showModal instead of redirecting the user
    # showModal(modalDialog(
    #     title = "Restore Session",
    #     "The session data was uploaded to the server. Please visit:",
    #     tags$a(restoreURL, href = restoreURL),
    #     "to restore the session"
    # ))

  })
}
