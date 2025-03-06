
#' Test the species module
#'
#' @noRd
#' @examples
#' mod_species_test()
#' mod_species_test(test_df_loaded())   # As if re-loading from previous run

mod_species_test <- function(df_loaded = NULL) {

  ui <- ui_setup(mod_species_ui(id = "test"))
  server <- function(input, output, session) {
    shinyOptions("file_dir" = "inst/extdata/")

    volumes <- server_setup()

    mod_species_server(id = "test", reactive(df_loaded))
  }

  shinyApp(ui, server)
}

mod_species_ui <- function(id) {

  ns <- NS(id)

  tabPanel(
    "Species Information",
    column(
      width = 12,
      div(
        id = "form_sp_info",

        h2("Species Information"),
        p("The basic details input in this section will be used in the generated",
          "report. The selected taxonomic group will determine which taxa specific",
          "vulnerability factors (Section C) are displayed."),

        textInput(ns("assessor_name"), labelMandatory("Assessor Name"), ""),
        textInput(ns("geo_location"), labelMandatory("Geographic Area Assessed")),
        selectInput(ns("tax_grp"), labelMandatory("Major Taxonomic Group"),
                    c("Vascular Plant", "Nonvascular Plant", "Lichen",
                      "Invert-Insect", "Invert-Mollusk", "Invert-Other",
                      "Fish", "Amphibian", "Reptile", "Mammal", "Bird")),
        textInput(ns("species_name"), labelMandatory("Species Scientific Name")),
        textInput(ns("common_name"), "Common Name"),
        br(),

        h3("Special kinds of species"),
        p("If the assessed species falls under one of the special cases below,",
          "check the appropriate box. Checking these boxes will tailor the",
          "calculation of the index to these special cases. See the ",
          a("NatureServe Guidelines",
            href = "https://www.natureserve.org/sites/default/files/guidelines_natureserveclimatechangevulnerabilityindex_r3.02_1_jun_2016.pdf", target="_blank"),
          " for more details."),
        checkboxInput(ns("cave"), "Species is an obligate of caves or groundwater systems"),
        checkboxInput(ns("mig"), "Species is migratory and you wish to enter exposure data for the migratory range that lies outside of the assessment area"),

        actionButton(ns("continue"), "Next", class = "btn-primary"),
        br(),br()
      )
    )
  )
}

mod_species_server <- function(id, df_loaded, parent_session) {
  stopifnot(is.reactive(df_loaded))

  moduleServer(id, function(input, output, session) {


    # Setup --------------------

    # Continue Button
    observeEvent(input$continue, switch_tab("Spatial Data Analysis", parent_session))

    # Enable the Submit button when all mandatory fields are filled out
    observe({
      m <- c("assessor_name", "geo_location", "tax_grp", "species_name")
      track_mandatory(m, input)
    })


    # Restore data -----------------
    observeEvent(df_loaded(), {
      update_restored2(df_loaded(), section = "sp_info", session)
    })


    # UI --------------------------
    # Show/Hide Fields depending on inputs
    # TODO: Better as conditional panels?
    observe({
      req(input$tax_gr)
      tax_lg <- ifelse(input$tax_grp %in% c("Vascular Plant", "Nonvascular Plant"),
                       "Plant",
                       ifelse(
                         input$tax_grp == "Lichen",
                         "Lichen",
                         "Animal"
                       ))
      if(tax_lg == "Plant"){
        shinyjs::show("plant_only")
        shinyjs::show("plant_only2")
        shinyjs::hide("animal_only")
      }
      if(tax_lg == "Animal"){
        shinyjs::show("animal_only")
        shinyjs::hide("plant_only")
        shinyjs::hide("plant_only2")
      }
      if(tax_lg == "Lichen"){
        shinyjs::hide("animal_only")
        shinyjs::hide("plant_only")
        shinyjs::hide("plant_only2")
      }

    })

    species_data <- reactive({
      data.frame(species_name = input$species_name,
                 common_name = input$common_name,
                 geo_location = input$geo_location,
                 assessor_name = input$assessor_name,
                 tax_grp = input$tax_grp,
                 mig = input$mig,
                 cave = input$cave)
    })

    # Return -------------------------------------------------
    exportTestValues(
      "species_data" = species_data(),
      "cave" = input$cave
    )

    list("species_data" = species_data,
         "cave" = reactive(input$cave))
  })

}
