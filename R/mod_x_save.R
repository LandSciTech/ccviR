mod_save_ui <- function(id, title) {

  ns <- NS(id)

  div(
    id = "footer",
    style = "float:left;",
    div(style = "display:inline-block",
        shinySaveButton(ns("downloadData"), "Save progress", "Save app data as a csv file",
                        class = "btn-primary", icon = shiny::icon("save")),
        uiOutput(ns("status"), class = "button-status")
    ),
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

    qs <- reactive(widen_vuln_coms2(questions))

    out_data <- reactive({
      ind <- if(is_ready(index())) index() else NULL
      spat1 <- if(is_ready(spatial$spat_run())) spatial$spat_run() else NULL
      spat2 <- if(is_ready(spatial$spat_res())) spatial$spat_res() else NULL
      combine_outdata2(species_data(), qs(), spat1, spat2, ind)
    })

    output$status <- renderUI({ # Use UI when rendering HTML
      req(index(), qs())

      if(!index_match_qs(qs(), index())) {
        return(tagList(icon("xmark", style = "color:red"),
                       "Omitting Index Results", br(), "(Questions have changed)"))
      } else return(NULL)

    })

    # save the data to a file
    shinyFileSave(input, "downloadData", root = volumes, filetypes = "csv")

    observeEvent(input$downloadData, {
      if(!is.integer(input$downloadData)){
        filename <- parseSavePath(roots = volumes, input$downloadData)$datapath
        if(!stringr::str_detect(filename, "\\.csv$")){
          filename <- paste0(filename, ".csv")
        }
        saveAttempt <- tryCatch({
          # Use blank for NA, so that NA in other fields can be kept as text
          # (e.g., what if someone uses NA for North America?)
          utils::write.csv(out_data(), filename, row.names = FALSE, na = "")},
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
      isolate(utils::write.csv(out_data(), file_nm, row.names = FALSE))
      message("Temporary file saved to:\n ", file_nm, "\n This will be deleted after R is closed")
      stopApp()
    })

    # Return --------------------------------------
    exportTestValues(out_data = out_data())

    out_data
  })
}
