

#Saving #=======================================================================
save_bookmark_ui <- function(id){
  fluidPage(
    strong("Choose location to save progess"),
    br(),
    shinyDirButton(NS(id, "save_dir"), "Location to create folder",
                   "Location to create folder"),
    br(),
    textInput(NS(id, "new_dir_name"),
              "Choose a name for the new folder that will be created"),
    br(),
    actionButton(NS(id, "save_action"), "Save")
  )
}

save_bookmark_server <- function(id, latestBookmarkURL, volumes){
  moduleServer(id, function(input, output, session) {

    shinyDirChoose(input, "save_dir", root = volumes)

    save_dir_pth <- reactive(parseDirPath(volumes, input$save_dir))

    onRestored(function(state) {
      showNotification(paste("Restored session:", basename(state$dir)),
                       duration = 10, type = "message")
    })

    setBookmarkExclude(c("save_dir"))

    observeEvent(input$save_action, {
      session$doBookmark()
      if (input$new_dir_name != "") {

        # "Error: Invalid state id" when using special characters - removing them:
        tmp_session_name <- stringr::str_replace_all(input$new_dir_name,
                                                     "[^[:alnum:]]", "")
        # TODO: check if a valid filename is provided (e.g. via
        # library(shinyvalidate)) for better user feedback

      } else {
        tmp_session_name <- paste(req(latestBookmarkURL))
      }
      # create the new directory in the chosen location
      new_dir <- fs::dir_create(fs::path(save_dir_pth(), tmp_session_name))

      # move the files from where shiny saves them to where the user can find them
      fs::dir_copy(path = fs::path(".", "shiny_bookmarks", req(latestBookmarkURL)),
               new_path = new_dir,
               overwrite = TRUE)

    }, ignoreInit = TRUE)
  })
}

# Load #=======================================================================
load_bookmark_ui <- function(id){
  fluidPage(
    strong("Select the folder where the app was saved"),
    br(),
    shinyDirButton(NS(id, "load_dir"), "Select Folder",
                   "Location of folder with previous state"),
    actionButton(NS(id, "load_action"), "Load")
  )
}

load_bookmark_server <- function(id, volumes){
  moduleServer(id, function(input, output, session){
    shinyDirChoose(input, "load_dir", root = volumes)
    load_dir_pth <- reactive(parseDirPath(volumes, input$load_dir))

    setBookmarkExclude(c("load_dir"))

    # LOAD SESSION
    observeEvent(input$load_action, {
      sessionName <- fs::path_file(load_dir_pth())

      targetPath <- file.path(".", "shiny_bookmarks", sessionName)

      if (!dir.exists(dirname(targetPath))) {
        dir.create(dirname(targetPath), recursive = TRUE)
      }

      # copy the bookmark to where shiny expects it to be
      fs::dir_copy(path = load_dir_pth(),
               new_path = targetPath,
               overwrite = TRUE)

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
  })
}
