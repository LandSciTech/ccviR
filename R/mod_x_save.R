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

mod_save_server <- function(id, volumes, species_data, spatial_data) {

  stopifnot(is.reactive(species_data))
  stopifnot(is.reactive(spatial_data))

  moduleServer(id, function(input, output, session) {

    # Make out_data #========================================================
    out_data_lst <- reactiveValues()

    # TODO: Split between species and vulnerability
    observe({
      # TODO: Move to Vulnerabilityu Qs
      # res_df <- widen_vuln_coms(inputs$vuln_df, coms_df = inputs$coms_df)

      # TODO: add res_df back in
      out_data_lst$start <- bind_cols(species_data()) %>% #, res_df) %>%
        mutate(ccviR_version = utils::packageVersion("ccviR"))
    })

    observe(out_data_lst$spat <- spatial_data())

   # TODO: Goes in Calc index module
   #  observeEvent(index_res(), {
   #    req(index_res())
   #    message("index out_data")
   #    vuln_df <- purrr::map_dfr(index_res()$vuln_df, widen_vuln_coms,
   #                              coms_df = coms_df())
   #
   #    conf_df <- index_res() %>%
   #      select("scenario_name", "mc_results") %>%
   #      mutate(mc_results = purrr::map(.data$mc_results, ~.x$index %>%
   #                                       factor(levels = c( "EV", "HV", "MV", "LV", "IE")) %>%
   #                                       table() %>%
   #                                       prop.table() %>%
   #                                       as.data.frame(stringsAsFactors = FALSE) %>%
   #                                       `names<-`(c("index", "frequency")))) %>%
   #      pull(.data$mc_results) %>%
   #      purrr::map_dfr(~ mutate(.x, index = paste0("MC_freq_", .data$index)) %>%
   #                       tidyr::pivot_wider(names_from = "index",
   #                                          values_from = "frequency"))
   #
   #    ind_df <- data.frame(CCVI_index = index_res()$index,
   #                         CCVI_conf_index = index_res()$conf_index,
   #                         mig_exposure = index_res()$mig_exp,
   #                         b_c_score = index_res()$b_c_score,
   #                         d_score = index_res()$d_score)
   #
   #    out_data_lst$index <- bind_cols(ind_df, conf_df, vuln_df)
   #  })

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

  })
}
