
data_prep_app <- function(testmode_in, ...) {

  # CSS to use in the app
  appCSS <-
    ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "


  ui <- fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    title = "Data Preparation for the ccviR app",
    tags$head(tags$style(type = "text/css",
                         ".container-fluid {  max-width: 950px; /* or 950px */}")),
    div(id = "header",
        h1("Data Preparation for the ccviR app"),
        strong(
          span("App developer: Sarah Endicott"), HTML("&bull;"),
          span("Project lead: Ilona Naujokaitis-Lewis"), HTML("&bull;"),
          span("With support from: ECCC"),
          br(),
          span("Code"),
          a("on GitHub", href = "https://github.com/see24/ccviR"),
          HTML("&bull;"),
          a("NatureServe website", href = "https://www.natureserve.org/conservation-tools/climate-change-vulnerability-index"))
    ),
    fluidRow(data_prep_ui("data_prep_mod"),
             br(),
             shinycssloaders::withSpinner(verbatimTextOutput("data_prep_msg",
                                                             placeholder = TRUE)),
             actionButton("data_reset", "Add Another Scenario"),
             br(),
             actionButton("data_done", "Close", class = "btn-primary")
    )
  )

  server <- function(input, output, session) {
    # Data Preparation #============================
    prepped_data <- data_prep_server("data_prep_mod")

    output$data_prep_msg <- renderText(prepped_data())

    observeEvent(input$data_done, {
      stopApp()
    })


    observeEvent(input$data_reset,{
      # File path ids to use with file choose
       c("clim_scn_nm", "clim_fut_period", "clim_em_scenario",
         "mat_fut_pth", "cmd_fut_pth", "ccei_pth",
         "map_pth", "mwmt_pth", "mcmt_pth", "clim_poly_pth") %>%
        purrr::map(~paste0("data_prep_mod-", .x)) %>%
        purrr::map(shinyjs::reset)

      shinyjs::runjs("window.scrollTo(0, 0)")
    })
  }

  shinyApp(ui, server, options = list(...))

}
