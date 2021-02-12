# based on example app: https://github.com/daattali/shiny-server/blob/master/mimic-google-form/app.R
# and blog post explaining it: https://deanattali.com/2015/06/14/mimicking-google-form-shiny/
library(shiny)

# which fields get saved
fieldsAll <- c("assessor_name", "geo_location", "tax_grp", "species_name",
               "common_name", "clim_var_dir")

# which fields are mandatory
fieldsMandatory <- c("assessor_name", "geo_location", "tax_grp", "species_name")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# save the results to a file
saveData <- function(data, fileName) {
  write.csv(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = TRUE)
}

# load all responses into a data.frame
# loadData <- function() {
#   files <- list.files(file.path(responsesDir), full.names = TRUE)
#   data <- lapply(files, read.csv, stringsAsFactors = FALSE)
#   #data <- dplyr::rbind_all(data)
#   data <- do.call(rbind, data)
#   data
# }

# directory where responses get stored
responsesDir <- file.path("responses")

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    title = "ccviR app",

    div(id = "header",
        h1("An app to run NatureServe CCVI process"),
        h4("subtitle"),
        strong(
          span("Created by Sarah Endicott"),
          HTML("&bull;"),
          span("Code"),
          a("on GitHub", href = "https://github.com/see24/ccviR"),
          HTML("&bull;"),
          a("NatureServe website", href = "https://www.natureserve.org/conservation-tools/climate-change-vulnerability-index"))
    ),

    fluidRow(
      column(width = 12,
             div(
               id = "form_sp_info",
               h3("Species information"),

               textInput("assessor_name", labelMandatory("Assessor Name"), ""),
               textInput("geo_location", labelMandatory("Geographic Area Assessed")),
               selectInput("tax_grp", labelMandatory("Major Taxonomic Group"),
                           c("Vascular Plant", "Nonvascular Plant", "Lichen",
                             "Invert-Insect", "Invert-Mollusk", "Invert-Other",
                             "Fish", "Amphibian", "Reptile", "Mammal", "Bird")),
               textInput("species_name", labelMandatory("Species Scientific Name")),
               textInput("common_name", "Common Name"),
               actionButton("submit", "Submit", class = "btn-primary"),

               shinyjs::hidden(
                 span(id = "submit_msg", "Submitting..."),
                 div(id = "error",
                     div(br(), tags$b("Error2: "), span(id = "error_msg"))
                 )
               )
             ),

             shinyjs::hidden(
               div(
                 id = "thankyou_msg",
                 h3("Thanks, your response was submitted successfully!"),
                 actionLink("submit_another", "Submit another response")
               )
             )
      )
    ),
    fluidRow(
      column(6,
             div(
               id = "secA",
               h3("Section A: Exposure to Local Climate Change (Calculate for species' range within assessment area)"),
               textInput("clim_var_dir", "Folder location of climate data",
                         placeholder = "C:/Users/path/to/folder"),
               textInput("range_poly_pth", "Range polygon shapefile"),
               textInput("nonbreed_poly_pth", "Non-breeding Range polygon shapefile",
                         placeholder = "C:/Users/path/to/file.shp"),
               textInput("assess_poly_pth", "Assessment area polygon shapefile",
                         placeholder = "C:/Users/path/to/file.shp"),
               textInput("hs_rast_pth", "Habitat suitability raster file",
                         placeholder = "C:/Users/path/to/file.tif"),
               actionButton("loadSpatial", "Load", class = "btn-primary")
             )
      ),
      column(6,
             shinyjs::hidden(
               div(
                 id = "map",
                 uiOutput("mapPanel")
               )
             )
      )
    )
  ),
  server = function(input, output, session) {

    # Enable the Submit button when all mandatory fields are filled out
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)

      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })

    # Gather all the form inputs (and add timestamp)
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, timestamp = epochTime())
      data <- t(data)
      data
    })

    # When the Submit button is clicked, submit the response
    observeEvent(input$submit, {

      # User-experience stuff
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")

      # Save the data (show an error message in case of error)
      tryCatch({
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })
    })

    # submit another response
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })

    observeEvent(input$loadSpatial, {
      shinyjs::show("map")
    })

    # render the admin panel
    output$mapPanel <- renderUI({

      div(
        id = "range_map",
        h2("Range map"),
        br(), br(),
        tmap::tmapOutput("range_map"), br(),
      )
    })

    # Make map
    output$range_map <- tmap::renderTmap({
      root_pth <- input$clim_var_dir

      clim_vars <- list(
        mat = list.files(root_pth,
                         pattern = "MAT.*tif$",
                         full.names = TRUE) %>% raster(),

        map = list.files(root_pth,
                         pattern = "MAP.*tif$",
                         full.names = TRUE) %>% raster(),

        tundra = list.files(root_pth,
                            pattern = "tundra.*shp$",
                            full.names = TRUE) %>%
          st_read(agr = "constant", quiet = TRUE),

        cmd = list.files(root_pth,
                         pattern = "CMD.*tif$",
                         full.names = TRUE) %>% raster(),

        ccei = list.files(root_pth,
                          pattern = "ccei.*tif$",
                          full.names = TRUE) %>% raster(),

        htn = list.files(root_pth,
                         pattern = "MWMT.*tif$",
                         full.names = TRUE) %>% raster()

      )

      range_poly <- sf::st_read(input$range_poly_pth)

      tmap::qtm(clim_vars$mat)+
        tmap::qtm(clim_vars$tundra, shape.col = "red", fill = NULL)+
        tmap::qtm(range_poly, fill = NULL)
    })





    # Allow user to download responses
    output$downloadBtn <- downloadHandler(
      filename = function() {
        sprintf("mimic-google-form_%s.csv", humanTime())
      },
      content = function(file) {
        write.csv(loadData(), file, row.names = FALSE)
      }
    )
  }
)
