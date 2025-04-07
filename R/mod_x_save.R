mod_save_ui <- function(id, title) {

  ns <- NS(id)

  div(
    id = "footer",
    style = "float:right; margin-top: 2rem;padding-right: 15px;",
    shinySaveButton(ns("downloadData"), "Save progress", "Save app data as a csv file",
                    class = "btn-primary", icon = shiny::icon("save")),
    br(),
    br(),
    br()
  )
}

mod_save_server <- function(id, volumes, species_data, spatial, questions,
                            index) {

  stopifnot(is.reactive(species_data))
  purrr::map(spatial, ~stopifnot(is.reactive(.x)))
  # Because when first loading these modules, we load the save module first,
  # `index doesn't actually exist yet (so don't test it).
  #stopifnot(is.reactive(index))
  purrr::map(questions, ~stopifnot(is.reactive(.x)))

  moduleServer(id, function(input, output, session) {

    # Make out_data #========================================================

    out_data <- reactive({
      ind <- if(is_ready(index())) index() else NULL
      spat1 <- if(is_ready(spatial$spat_run())) spatial$spat_run() else NULL
      spat2 <- if(is_ready(spatial$spat_res())) spatial$spat_res() else NULL
      combine_outdata2(species_data(), questions, spat1, spat2, ind)
    })

    exportTestValues(out_data = out_data())

    # save the data to a file
    shinyFileSave(input, "downloadData", root = volumes, filetypes = "csv")

    observeEvent(input$downloadData, {
      if(!is.integer(input$downloadData)){
        filename <- parseSavePath(roots = volumes, input$downloadData)$datapath
        if(!stringr::str_detect(filename, "\\.csv$")){
          filename <- paste0(filename, ".csv")
        }
        saveAttempt <- tryCatch({
          write.csv(out_data(), filename, row.names = FALSE)},
          error = function(e){
            showModal(modalDialog(
              p("File could not be saved. Is it open?"),
              footer = tagList(
                actionButton("retry", "Retry"),
                modalButton("Cancel")
              ),
              title = "Error Permission Denied"))
            print(conditionMessage(e))
          })
      }
    })

    # Retry save if there was an error due to open file
    observeEvent(input$retry, {
      # doesn't quite work but better than nothing
      removeModal()
      shinyjs::click("downloadData")
    })


    # NOTE: Remove if deployed to a server with multiple users possible. Will
    # stop for all users. Not an issue when run locally
    session$onSessionEnded(function() {
      # save the csv to a temp file is case user forgot to save
      file_nm <- paste0("ccviR_temp_save_",
                        Sys.time() %>% format() %>%
                          stringr::str_replace_all("\\W", "_"),
                        "_")
      file_nm <- tempfile(pattern = file_nm, fileext = ".csv")
      isolate(write.csv(out_data(), file_nm, row.names = FALSE))
      message("Temporary file saved to:\n ", file_nm, "\n This will be deleted after R is closed")
      stopApp()
    })

    # Return --------------------------------------
    reactive({
      if(is_ready(out_data())) {
        combine_outdata2(out_data_lst)
      } else NULL
    })

    out_data
  })
}
