library(shiny)
library(shinyjs)
library(utils)
library(tools)
library(stringi)

ui <- function(request) {
  fluidPage(
    useShinyjs(),
    textInput("control_label", "This controls some of the labels:", "LABEL TEXT"),
    numericInput("inNumber", "Number input:", min = 1, max = 20, value = 5, step = 0.5 ),
    radioButtons("inRadio", "Radio buttons:", c("label 1" = "option1", "label 2" = "option2", "label 3" = "option3")),
    fileInput("restore_bookmark", "Restore Session", multiple = FALSE, accept = ".rds"),
    actionButton("save_inputs", 'Save Session', icon = icon("download"))
  )
}

server <-  function(input, output, session) {
  latestBookmarkURL <- reactiveVal()

  onBookmarked(
    fun = function(url) {
      latestBookmarkURL(parseQueryString(url))
    }
  )

  onRestored(function(state) {
    showNotification(paste("Restored session:", basename(state$dir)), duration = 10, type = "message")
  })

  observeEvent(input$save_inputs, {
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
        tmp_session_name <- stri_replace_all(tmp_session_name, "", regex = "[^[:alnum:]]")
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

  # LOAD SESSION
  observeEvent(input$restore_bookmark, {

    sessionName <- file_path_sans_ext(input$restore_bookmark$name)
    targetPath <- file.path(".", "shiny_bookmarks", sessionName, "input.rds")

    if (!dir.exists(dirname(targetPath))) {
      dir.create(dirname(targetPath), recursive = TRUE)
    }

    # copy the bookmark to where shiny expects it to be
    file.copy(
      from = input$restore_bookmark$datapath,
      to = targetPath,
      overwrite = TRUE
    )

    restoreURL <- paste0(session$clientData$url_protocol, "//", session$clientData$url_hostname, ":", session$clientData$url_port, "/?_state_id_=", sessionName)

    # redirect user to restoreURL
    runjs(sprintf("window.location = '%s';", restoreURL))

  })

}

shinyApp(ui, server, enableBookmarking = "server")
