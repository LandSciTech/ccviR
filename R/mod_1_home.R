
#' Test the home module
#'
#' @noRd
#' @examples
#' mod_home_test()

mod_home_test <- function() {
  ui <- ui_setup(mod_home_ui(id = "test"))
  server <- function(input, output, session) {
    shinyOptions("file_dir" = system.file("extdata/", package = "ccviR"))
    volumes <- server_setup()
    #cat(volumes)
    #cat(getwd())
    mod_home_server(id = "test", volumes = volumes)
  }

  shinyApp(ui, server)
}

mod_home_ui <- function(id) {

  ns <- NS(id)

  tabPanel(
    "Welcome", value = "home",
    fluidPage(
      h2("Welcome"),
      p("The ccviR app provides a new interface for the Climate Change Vulnerability Index (CCVI) created by ",
        a("NatureServe", href = "https://www.natureserve.org/conservation-tools/climate-change-vulnerability-index", target="_blank"),
        "that automates the spatial analysis needed to inform the index. ",
        "The app is based on version 3.02 of the NatureServe CCVI. ",
        "For a demonstration of how to use the app, see the app ",
        a("tutorial.", href = "https://landscitech.github.io/ccviR/articles/app_vignette.html", target="_blank"),),
      p("The NatureServe CCVI scores the vulnerability of a species to climate change based on:"),
      tags$ul(
        tags$li("The species' predicted exposure to climate change -", strong("Section A")),
        tags$li("Factors associated with the species' climate change sensitivity, including:"),
        tags$ul(
          tags$li("Indirect exposure to climate change -", strong("Section B")),
          tags$li("Species-specific sensitivity and adaptive capacity factors -", strong("Section C")),
          tags$li("Documented and modeled response to climate change -", strong("Section D")),)),
      p("For more information about the index see the ",
        a("NatureServe Guidelines.", href = "https://www.natureserve.org/sites/default/files/guidelines_natureserveclimatechangevulnerabilityindex_r3.02_1_jun_2016.pdf", target="_blank"),),
      h3("Preparing to use the app"),
      p(strong("Step 0: "),"The first time you use the app you can either",
        a("download", href = "https://drive.google.com/drive/folders/18mO5GDUmwi-nswhIAC36bmtYsvmqNQkH?usp=share_link", target="_blank"),
        "a pre-prepared climate data set used in the app or",
        " prepare your own using raw climate data and the ",
        a("data preparation app.", href = "https://landscitech.github.io/ccviR/articles/data_prep_vignette.html", target="_blank")),
      p(strong("Step 1: "), "Acquire species-specific spatial datasets
              (required datasets are indicated with" , labelMandatory("a"), "):",
        tags$ul(
          tags$li(labelMandatory("Species North American or global range polygon")),
          tags$li(labelMandatory("Assessment area polygon")),
          tags$li("Non-breeding range polygon"),
          tags$li("Projected range change raster"),
          tags$li("Physiological thermal niche (PTN) polygon - ",
                  "polygon should include cool or cold environments ",
                  "that the species occupies that may be lost or reduced ",
                  "within the assessment area as a result of climate change"))),
      p(strong("Step 2: "), "Acquire species-specific sensitivity and life history data.",
        "Including information about dispersal and movement ability, ",
        "temperature/precipitation regime, dependence on disturbance events, ",
        "relationship with ice or snow-cover habitats, physical",
        " specificity to geological features or their derivatives, ",
        "interactions with other species including diet and pollinator ",
        "specificity, genetic variation, and phenological response to ",
        "changing seasons. Recognizing that some of this information is",
        " unknown for many species, the Index is designed such that only",
        " 10 of the 19 sensitivity factors require input in order to ",
        "obtain an overall Index score."),
      p(strong("Note: "), "The app will NOT save your progress automatically.
              Be sure to save your progress throughout the assessment to prevent
              loss of data. The state of the app can be saved by clicking the
              \"Save progress\" button at the bottom of the app at any point during
              the assessment. Refreshing the app or timing out will result in
              progress being lost."),
      h3("Start assessment"),
      actionButton(ns("continue"), "Start", class = "btn-primary"),
      br(),
      br(),
      strong("Or load data from a previous assessment"),
      br(),
      #load_bookmark_ui("load"),
      shinyFilesButton(ns("loadcsv"), "Select file", "Select file", multiple = FALSE),
      textOutput(ns("ui_loaded"), inline = TRUE),
      br(),
      br(),
      p("Download column definitions for saved data"),
      downloadButton(ns("downloadDefs"), "Download csv"),
      br(),
      h3("Citation"),
      p("Endicott S, Naujokaitis-Lewis I (2023). ",
        em("ccviR: Calculate the NatureServe Climate Change Vulnerability ",
           "Index in R. "),
        "Environment and Climate Change Canada, Science and Technology ",
        "Branch. ",
        a("https://landscitech.github.io/ccviR/.", href = "https://landscitech.github.io/ccviR/")),
      h3("References"),
      p("Young, B. E., K. R. Hall, E. Byers, K. Gravuer, G. Hammerson,",
        " A. Redder, and K. Szabo. 2012. Rapid assessment of plant and ",
        "animal vulnerability to climate change. Pages 129-150 in ",
        "Wildlife Conservation in a Changing Climate, edited by J. ",
        "Brodie, E. Post, and D. Doak. University of Chicago Press, ",
        "Chicago, IL."),
      p("Young, B. E., N. S. Dubois, and E. L. Rowland. 2015. Using the",
        " Climate Change Vulnerability Index to inform adaptation ",
        "planning: lessons, innovations, and next steps. Wildlife ",
        "Society Bulletin 39:174-181.")
    )
  )
}

mod_home_server <- function(id, volumes, parent_session) {

  moduleServer(id, function(input, output, session) {


    #outputOptions(output, "", suspendWhenHidden = FALSE)

    observeEvent(input$continue, switch_tab("species", parent_session))

    # restore a previous session
    shinyFileChoose("loadcsv", root = volumes, input = input,
                    filetypes = "csv")

    # Restore from saved file
    path <- reactive(parse_path(volumes, input$loadcsv))

    df_loaded <- reactive(load_previous(path()))

    output$ui_loaded <- renderText({
      req(input$loadcsv)
      req(df_loaded())
      paste0("Dataset loaded: ", path())
    })

    output$downloadDefs <- downloadHandler(
      filename = "CCVI_column_definitions_results.csv",
      content = function(file) {
        out <- utils::read.csv(system.file("extdata/column_definitions_results.csv",
                                           package = "ccviR"))
        write.csv(out, file, row.names = FALSE)
      }
    )


    # Return -------------------------------------------------
    list("df_loaded" = df_loaded)
  })

}
