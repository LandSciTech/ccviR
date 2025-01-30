mod_save_ui <- function(id, title) {

  ns <- NS(id)

  div(
    id = "footer",
    style = "float:right",
    br(), br(), br(), br(),
    shinySaveButton(ns("downloadData"), "Save progress", "Save app data as a csv file",
                    class = "btn-primary", icon = shiny::icon("save")),
    br(),
    br(),
    br()
  )
}

mod_save_server <- function(id, volumes, species_data, spatial_data, questions,
                            index) {

  stopifnot(is.reactive(species_data))
  stopifnot(is.reactive(spatial_data))
  purrr::map(questions, ~stopifnot(is.reactive(.x)))

  moduleServer(id, function(input, output, session) {

    # Make out_data #========================================================
    out_data_lst <- reactiveValues()


    observe({
      out_data_lst$start <- bind_cols(
        species_data(),
        widen_vuln_coms2(questions)) %>%
        mutate(ccviR_version = utils::packageVersion("ccviR"))
    })

    observe(out_data_lst$spat <- spatial_data())
    observe(out_data_lst$index <- index())


    exportTestValues(out_data = shiny::reactiveValuesToList(out_data_lst),
                     doSpatial = doSpatial())

    # save the data to a file
    shinyFileSave(input, "downloadData", root = volumes, filetypes = "csv")

    observeEvent(input$downloadData, {
      if(!is.integer(input$downloadData)){
        filename <- parseSavePath(roots = volumes, input$downloadData)$datapath
        if(!stringr::str_detect(filename, "\\.csv$")){
          filename <- paste0(filename, ".csv")
        }
        saveAttempt <- tryCatch({
          write.csv(combine_outdata(reactiveValuesToList(out_data_lst)), filename,
                    row.names = FALSE)},
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
      isolate(
        write.csv(combine_outdata(reactiveValuesToList(out_data_lst)),
                  file_nm,
                  row.names = FALSE)
      )
      message("Temporary file saved to:\n ", file_nm, "\n This will be deleted after R is closed")
      stopApp()
    })

  })
}
