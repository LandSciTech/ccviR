# based on example app: https://github.com/daattali/shiny-server/blob/master/mimic-google-form/app.R
# and blog post explaining it: https://deanattali.com/2015/06/14/mimicking-google-form-shiny/

#' Create the ccviR Shiny application
#'
#' @noRd
#' @examples
#'
#' ccvi_app2()

ccvi_app2 <- function(testmode_in, ...){

  # Input options
  valueNms <- c("Greatly increase", "Increase", "Somewhat increase", "Neutral")
  valueOpts <- c(3, 2, 1, 0)

  # set theme
  my_theme <- ggplot2::theme_classic() +
    ggplot2::theme(text = ggplot2::element_text(size = 12),
          strip.background = ggplot2::element_blank())

  ggplot2::theme_set(my_theme)

  ui <- ui_setup(
    mod_home_ui(id = "home"),
    mod_species_ui(id = "species"),
    mod_spatial_ui(id = "spatial")
    #mod_A_ui(id = "section_a"),
    #mod_B_ui(id = "section_b"),
    #mod_C_ui(id = "section_c")
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
      parent_session = session)
    #x <- mod_A_server(id = "section_a")

    mod_save_server(
      id = "save", volumes,
      sp$species_data,
      spatial$spatial_data)
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

  # start up Note this time out is because when I disconnected from VPN it
  # made the getVolumes function hang forever because it was looking for
  # drives that were no longer connected. Now it will give an error
  timeout <- R.utils::withTimeout({
    volumes <- c(wd = "inst/extdata/",# TODO: revert: getShinyOption("file_dir"),
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
