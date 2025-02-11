# based on example app: https://github.com/daattali/shiny-server/blob/master/mimic-google-form/app.R
# and blog post explaining it: https://deanattali.com/2015/06/14/mimicking-google-form-shiny/

#' Create the ccviR Shiny application
#'
#' @noRd
#' @examples
#'
#' ccvi_app2()

ccvi_app2 <- function(input_files = NULL, ...){

  # MOVED TO lookup_tbls.R so available to all functions
  # Input options
  #valueNms <- c("Greatly increase", "Increase", "Somewhat increase", "Neutral")
  #valueOpts <- c(3, 2, 1, 0)



  ui <- ui_setup(
    mod_home_ui(id = "home"),
    mod_species_ui(id = "species"),
    mod_spatial_ui(id = "spatial"),
    mod_A_ui(id = "section_a"),
    mod_B_ui(id = "section_b"),
    mod_C_ui(id = "section_c"),
    mod_D_ui(id = "section_d"),
    mod_results_ui(id = "results"),
  ) # Note: mod_save_ui() is inside ui_setup()

  server <- function(input, output, session) {
    volumes <- server_setup()

    restore <- mod_home_server(id = "home", volumes, parent_session = session)

    sp <- mod_species_server(
      id = "species",
      df_loaded = restore$df_loaded,
      parent_session = session)

    spatial <- mod_spatial_server(
      id = "spatial", volumes,
      df_loaded = restore$df_loaded,
      cave = sp$cave,
      parent_session = session,
      input_files = input_files)

    mod_A_server(
      id = "section_a",
      spatial_details = spatial$spatial_details,
      parent_session = session)

    b <- mod_B_server(
      id = "section_b",
      df_loaded = restore$df_loaded,
      parent_session = session)

    c <- mod_C_server(
      id = "section_c",
      df_loaded = restore$df_loaded,
      spatial_details = spatial$spatial_details,
      parent_session = session)

    d <- mod_D_server(
      id = "section_d",
      df_loaded = restore$df_loaded,
      spatial_details = spatial$spatial_details,
      parent_session = session)

    index <- mod_results_server(
      id = "results",
      df_loaded = restore$df_loaded,
      species_data = sp$species_data,
      spatial_details = spatial$spatial_details,
      questions = c(b, c, d)
    )

    mod_save_server(
      id = "save", volumes,
      sp$species_data,
      spatial$spatial_data,
      questions = c(b, c, d),
      index = index$index)
  }


  onStop(function(){options(testmode_in)})

  shinyApp(ui, server, enableBookmarking = "server",
           options = list(...))
}

ui_setup <- function(...) {
  # CSS to use in the app
  appCSS <-
    ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
   .shiny-output-error-validation {color: #d9534f; font-weight: bold;}
  "

  fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    title = "ccviR app",
    tags$head(tags$style(type = "text/css",
                         ".container-fluid {  max-width: 1050px; /* or 1050px */}")),
    div(id = "header",
        h1("ccviR: An app to calculate the NatureServe Climate Change Vulnerability Index"),
        strong(
          span("ccviR is a product developed and maintained by ECCC STB. This project is lead by Ilona Naujokaitis-Lewis and Sarah Endicott"),
          br(),
          span("Code"),
          a("on GitHub", href = "https://github.com/see24/ccviR", target="_blank"),
          HTML("&bull;"),
          a("ccviR website", href = "https://landscitech.github.io/ccviR/articles/app_vignette.html", target="_blank"),
          HTML("&bull;"),
          a("NatureServe website", href = "https://www.natureserve.org/conservation-tools/climate-change-vulnerability-index", target="_blank"))
    ),
    navlistPanel(
      id = "tabset",
      well = FALSE,
      widths = c(3, 9),
      ...
    ),
    mod_save_ui("save")
  )
}


server_setup <- function() {
  file_pths <- NULL

  #TODO: Remove development mode
  shinyOptions("file_dir" = "inst/extdata")

  if(is_testing()) {
    shinyOptions("file_dir" = system.file("extdata/", package = "ccviR"))
  }


  # start up Note this time out is because when I disconnected from VPN it
  # made the getVolumes function hang forever because it was looking for
  # drives that were no longer connected. Now it will give an error
  timeout <- R.utils::withTimeout({
    volumes <- c(wd = getShinyOption("file_dir"),
                 Home = fs::path_home(),
                 getVolumes()())
  }, timeout = 200, onTimeout = "silent")

  if(is.null(timeout)){
    stop("The app is unable to access your files because you were connected",
         " to the VPN and then disconnected. To fix this either reconnect to",
         " the VPN or restart your computer and use the app with out connecting",
         " to VPN. See issue https://github.com/see24/ccviR/issues/36 for more ",
         "information", call. = FALSE)
  }

  volumes
}
