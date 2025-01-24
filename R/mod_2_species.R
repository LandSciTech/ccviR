mod_species_ui <- function(id, title) {

  ns <- NS(id)

  tabPanel(
    "Species Information",
    column(
      width = 12,
      div(
        id = "form_sp_info",
        h2("Species Information"),
        p("The basic details input in this section will be used in the generated
                report. The selected taxonomic group will determine which taxa specific
                vulnerability factors (Section C) are displayed."),
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
        p("If the assessed species falls under one of the special cases below,
                check the appropriate box. Checking these boxes will tailor the
                calculation of the index to these special cases. See the ",
          a("NatureServe Guidelines",
            href = "https://www.natureserve.org/sites/default/files/guidelines_natureserveclimatechangevulnerabilityindex_r3.02_1_jun_2016.pdf", target="_blank"),
          " for more details."),
        checkboxInput(ns("cave"), "Species is an obligate of caves or groundwater systems"),
        checkboxInput(ns("mig"), "Species is migratory and you wish to enter exposure data for the migratory range that lies outside of the assessment area"),
        actionButton(ns("next"), "Next", class = "btn-primary"),
        br(),br()

      )
    )
  )
}

mod_species_server <- function(id, df_loaded) {
  stopifnot(is.reactive(df_loaded))

  moduleServer(id, function(input, output, session) {

    observeEvent(df_loaded(), {
      update_restored(df_loaded(), section = "sp_info", session)
    })

    # Mandatory fields
    fieldsMandatory <- c("assessor_name", "geo_location", "tax_grp", "species_name")

    # Species Info #=================
    # Enable the Submit button when all mandatory fields are filled out
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)

      shinyjs::toggleState(id = "next1", condition = mandatoryFilled)
    })

    # When next button is clicked move to next panel
    observeEvent(input$next1, {
      updateTabsetPanel(session, "tabset",
                        selected = "Spatial Data Analysis"
      )
      shinyjs::runjs("window.scrollTo(0, 0)")
    })

    observe({
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


    # Return to app ---------------------------------------------
#    tax_lg

  })

}
