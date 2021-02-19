# based on example app: https://github.com/daattali/shiny-server/blob/master/mimic-google-form/app.R
# and blog post explaining it: https://deanattali.com/2015/06/14/mimicking-google-form-shiny/
library(shiny)
library(shinyFiles)
library(tmap)
library(tidyr)
devtools::load_all()
#library(ccviR)

# which fields get saved
fieldsAll <- c("assessor_name", "geo_location", "tax_grp", "species_name",
               "common_name", "clim_var_dir")

# which fields are mandatory
fieldsMandatory <- c("assessor_name", "geo_location", "tax_grp", "species_name")

# File path ids to use with file choose
filePathIds <- c("range_poly_pth", "nonbreed_poly_pth", "assess_poly_pth",
                 "hs_rast_pth")

# Input options
valueNms <- c("Greatly increase", "Increase", "Somewhat increase", "Neutral")
valueOpts <- c(3, 2, 1, 0)

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# format multiple values from checkbox
getMultValues <- function(x, nm){
  # if(length(x) == 0){
  #   return(data.frame(Code = nm, Value1 = NA_real_, Value2 = NA, Value3 = NA, Value4 = NA,
  #                     stringsAsFactors = FALSE))
  # }
  # df <- data.frame(val = as.numeric(x), Code = nm, N = 1:length(x),
  #                  stringsAsFactors = FALSE) %>%
  #   pivot_wider(id_col = Code, names_from = N,
  #               values_from = val, names_prefix = "Value")
  x <- as.numeric(x)

  df <- data.frame(Code = nm, Value1 = x[1], Value2 = x[2], Value3 = x[3],
                   Value4 = x[4], stringsAsFactors = FALSE)

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

  # Header #=================================
ui <-  fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    title = "ccviR app",

    div(id = "header",
        h1("An app to run the NatureServe CCVI process"),
        h4("subtitle"),
        strong(
          span("Created by Sarah Endicott"),
          HTML("&bull;"),
          span("Code"),
          a("on GitHub", href = "https://github.com/see24/ccviR"),
          HTML("&bull;"),
          a("NatureServe website", href = "https://www.natureserve.org/conservation-tools/climate-change-vulnerability-index"))
    ),
    # Species Info #===============
    tabsetPanel(
      id = "tabset",
      tabPanel(
        "Species Information",
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
                 checkboxInput("cave", "Check if the species is an obligate of caves or groundwater systems"),
                 checkboxInput("mig", "Check if species is migratory and you wish to enter exposure data for the migratory range that lies outside of the assessment area"),
                 actionButton("next1", "Next", class = "btn-primary"),
                 br(),br()

               )
        )),
        # Spatial Analysis #============
        tabPanel(
          "Spatial Data Analysis",
          column(4,
                 div(
                   id = "spatial",
                   h3("Spatial data analysis"),
                   strong("Folder location of climate data:"),
                   shinyDirButton("clim_var_dir", "Choose a folder",
                                  "Folder location of climate data"),
                   verbatimTextOutput("clim_var_dir", placeholder = TRUE),
                   br(),
                   strong("Range polygon shapefile:"),
                   shinyFilesButton("range_poly_pth", "Choose file",
                                    "Range polygon shapefile", multiple = FALSE),
                   verbatimTextOutput("range_poly_pth", placeholder = TRUE),
                   br(),
                   strong("Non-breeding Range polygon shapefile"),
                   shinyFilesButton("nonbreed_poly_pth", "Choose file",
                                    "Non-breeding Range polygon shapefile",
                                    multiple = FALSE),
                   verbatimTextOutput("nonbreed_poly_pth", placeholder = TRUE),
                   br(),
                   strong("Assessment area polygon shapefile"),
                   shinyFilesButton("assess_poly_pth", "Choose file",
                                    "Assessment area polygon shapefile",
                                    multiple = FALSE),
                   verbatimTextOutput("assess_poly_pth", placeholder = TRUE),
                   br(),
                   strong("Habitat suitability raster file"),
                   shinyFilesButton("hs_rast_pth", "Choose file",
                                    "Habitat suitability raster file", multiple = FALSE),
                   verbatimTextOutput("hs_rast_pth", placeholder = TRUE),
                   br(),
                   strong("Click Load to explore map or Next to proceed"),
                   br(),
                   actionButton("loadSpatial", "Load", class = "btn-primary"),
                   actionButton("next2", "Next", class = "btn-primary"),
                   br(), br()
                 )
          ),
          column(8,
                 shinyjs::hidden(
                   div(
                     id = "range_map",
                     h2("Range map"),
                     br(),
                     selectInput("rast_plot", "Raster to plot",
                                 list(Temperature = "mat",
                                      Precipitation = "map",
                                      Moisture = "cmd",
                                      `Climate change exposure index` = "ccei",
                                      `Historical thermal niche` = "htn",
                                      `Habitat suitability` = "hs_rast")),
                     selectInput("poly_plot", "Polygon to plot",
                                 list(`Non-breeding range` = "nonbreed_poly",
                                      `Assessment area`= "assess_poly",
                                      `Physiological thermal niche` = "tundra")),
                     br(),
                     tmap::tmapOutput("range_map")
                   )
                 )
          )
        ),
      # Section B questions #=================
      tabPanel(
        "Vulnerability Questions",
        fluidRow(
          column(12,
                 div(id = "secB",
                     h3("Section B: Indirect Exposure to Climate Change"),
                     h4("Evaluate for specific geographical area under consideration"),
                     h5("Factors that influence vulnerability (* at least three required)"),
                     actionButton("guideB", "Show guidelines"),
                     checkboxGroupInput("B1", "1) Exposure to sea level rise:",
                                        choiceNames = valueNms,
                                        choiceValues = valueOpts,
                                        inline = TRUE),
                     checkboxGroupInput("B2a", "2a) Distribution relative to natural barriers",
                                        choiceNames = valueNms,
                                        choiceValues = valueOpts,
                                        inline = TRUE),
                     checkboxGroupInput("B2b", "2b) Distribution relative to anthropogenic barriers",
                                        choiceNames = valueNms,
                                        choiceValues = valueOpts,
                                        inline = TRUE),

                     checkboxGroupInput("B1", "  3) Predicted impact of land use changes resulting from human responses to climate change",
                                        choiceNames = valueNms[2:4],
                                        choiceValues = valueOpts[2:4],
                                        inline = TRUE)
                 )
          )
        ),
        # Section C questions #=============================
        fluidRow(
          column(12,
                 div(id = "secC",
                     h3("Section C: Sensitivity and Adaptive Capacity"),
                     h5("(* at least 10 required)"),
                     actionButton("guideC", "Show guidelines"),
                     checkboxGroupInput("C1", "1) Dispersal and movements",
                                        choiceNames = valueNms,
                                        choiceValues = valueOpts,
                                        inline = TRUE),

                     strong("2a) Predicted sensitivity to changes in temperature:"),
                     checkboxGroupInput("C2ai", "i) historical thermal niche. Leave blank except to override the spatial analysis.",
                                        choiceNames = valueNms,
                                        choiceValues = valueOpts,
                                        inline = TRUE),
                     checkboxGroupInput("C2aii", "ii) physiological thermal niche. Leave blank except to override the spatial analysis.",
                                        choiceNames = valueNms,
                                        choiceValues = valueOpts,
                                        inline = TRUE),

                     strong("2b) Predicted sensitivity to changes in precipitation, hydrology, or moisture regime:"),
                     checkboxGroupInput("C2bi", "i)historical hydrological niche. Leave blank except to override the spatial analysis.",
                                        choiceNames = valueNms,
                                        choiceValues = valueOpts,
                                        inline = TRUE),
                     checkboxGroupInput("C2bii", "ii) physiological hydrological niche.",
                                        choiceNames = valueNms,
                                        choiceValues = valueOpts,
                                        inline = TRUE),

                     checkboxGroupInput("C2c", "2c) Dependence on a specific disturbance regime likely to be impacted by climate change.",
                                        choiceNames = valueNms[2:4],
                                        choiceValues = valueOpts[2:4],
                                        inline = TRUE),
                     checkboxGroupInput("C2d", "2d) Dependence on ice, ice-edge, or snow-cover habitats.",
                                        choiceNames = valueNms,
                                        choiceValues = valueOpts,
                                        inline = TRUE),

                     checkboxGroupInput("C3", "3) Restriction to uncommon landscape/geological features or derivatives.",
                                        choiceNames = valueNms[2:4],
                                        choiceValues = valueOpts[2:4],
                                        inline = TRUE),
                     checkboxGroupInput("C4a", "4a) Dependence on other species to generate required habitat.",
                                        choiceNames = valueNms[2:4],
                                        choiceValues = valueOpts[2:4],
                                        inline = TRUE),
                     checkboxGroupInput("C4b", "4b) Dietary versatility (animals only).",
                                        choiceNames = valueNms[2:4],
                                        choiceValues = valueOpts[2:4],
                                        inline = TRUE),
                     checkboxGroupInput("C4c", "4c) Pollinator versatility (plants only).",
                                        choiceNames = valueNms[2:4],
                                        choiceValues = valueOpts[2:4],
                                        inline = TRUE),
                     checkboxGroupInput("C4d", "4d) Dependence on other species for propagule dispersal.",
                                        choiceNames = valueNms[2:4],
                                        choiceValues = valueOpts[2:4],
                                        inline = TRUE),
                     checkboxGroupInput("C4e", "4e) Sensitivity to pathogens or natural enemies.",
                                        choiceNames = valueNms[2:4],
                                        choiceValues = valueOpts[2:4],
                                        inline = TRUE),
                     checkboxGroupInput("C4f", "4f) Sensitivity to competition from native or non-native species.",
                                        choiceNames = valueNms[2:4],
                                        choiceValues = valueOpts[2:4],
                                        inline = TRUE),
                     checkboxGroupInput("C4f", "4f) Forms part of an interspecific interaction not covered by 4a-f.",
                                        choiceNames = valueNms[2:4],
                                        choiceValues = valueOpts[2:4],
                                        inline = TRUE),

                     checkboxGroupInput("C5a", "5a) Measured genetic variation.",
                                        choiceNames = valueNms[2:4],
                                        choiceValues = valueOpts[2:4],
                                        inline = TRUE),
                     checkboxGroupInput("C5b", "5b) Occurrence of bottlenecks in recent evolutionary history (use only if 5a is unknown).",
                                        choiceNames = valueNms[2:4],
                                        choiceValues = valueOpts[2:4],
                                        inline = TRUE),
                     checkboxGroupInput("C5b", "5c) Reproductive system (plants only; use only if C5a and C5b are “unknown”).",
                                        choiceNames = valueNms[2:4],
                                        choiceValues = valueOpts[2:4],
                                        inline = TRUE),

                     checkboxGroupInput("C6", "6) Phenological response to changing seasonal temperature and precipitation dynamics.",
                                        choiceNames = valueNms[2:4],
                                        choiceValues = valueOpts[2:4],
                                        inline = TRUE)
                 )
          )
        ),
        # Section D questions #=============================
        fluidRow(
          column(12,
                 div(id = "secD",
                     h3("Section D: Documented or Modeled Response to Climate Change"),
                     h5("(Optional; May apply across the range of a species)"),
                     actionButton("guideD", "Show guidelines"),
                     checkboxGroupInput("D1", "1) Documented response to recent climate change. ",
                                        choiceNames = valueNms,
                                        choiceValues = valueOpts,
                                        inline = TRUE),
                     checkboxGroupInput("D2", "2) Modeled future (2050) change in population or range size.",
                                        choiceNames = valueNms,
                                        choiceValues = valueOpts,
                                        inline = TRUE),
                     checkboxGroupInput("D3", "3) Overlap of modeled future (2050) range with current range.",
                                        choiceNames = valueNms,
                                        choiceValues = valueOpts,
                                        inline = TRUE),
                     checkboxGroupInput("D4", "4) Occurrence of protected areas in modeled future (2050) distribution.",
                                        choiceNames = valueNms[2:4],
                                        choiceValues = valueOpts[2:4],
                                        inline = TRUE),
                     actionButton("submitVuln", "Submit", class = "btn-primary")
                 )
          )
        )
      ),
      # Results #===================================
      tabPanel(
        "Results",
        column(12,
               div(
                 id = "formData",
                 h3("Results"),
                 strong("Climate Change Vulnerability Index: "),
                 textOutput("index"),
                 strong("Confidence in index: "),
                 textOutput("conf_index"),
                 plotOutput("conf_graph", width = 200, height = 200)
                 #verbatimTextOutput("formData")
               ))
      )
    )
)


  # Server #========================
server <- function(input, output, session) {

  # start up Note this time out is because when I disconnected from VPN it
  # made the getVolumes function hang forever because it was looking for
  # drives that were no longer connected. Now it will give an error
  R.utils::withTimeout({
    volumes <- c(wd = "C:/Users/endicotts/Documents/Ilona/ccviR/data",
                 Home = fs::path_home(),
                 getVolumes()())
  }, timeout = 10, onTimeout = "error")


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
  })

  # get values to use later
  sp_nm <- reactive(input$species_name)
  scale_nm <- reactive(input$geo_location)

  # Spatial Analysis #===============
  # Find file paths
  shinyDirChoose(input, "clim_var_dir", root = volumes)
  purrr::map(filePathIds, shinyFileChoose, root = volumes, input = input)

  # output file paths
  output$clim_var_dir <- renderText({
    parseDirPath(volumes, input$clim_var_dir)
  })
  output$range_poly_pth <- renderText({
    parseFilePaths(volumes, input$range_poly_pth)$datapath
  })
  output$nonbreed_poly_pth <- renderText({
    parseFilePaths(volumes, input$nonbreed_poly_pth)$datapath
  })
  output$assess_poly_pth <- renderText({
    parseFilePaths(volumes, input$assess_poly_pth)$datapath
  })
  output$hs_rast_pth <- renderText({
    parseFilePaths(volumes, input$hs_rast_pth)$datapath
  })

  # load spatial data
  clim_vars <- reactive({
    root_pth <- parseDirPath(volumes, input$clim_var_dir)

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
    clim_vars
  })

  range_poly <- reactive({
    sf::st_read(parseFilePaths(volumes,
                               input$range_poly_pth)$datapath)
  })

  nonbreed_poly <- reactive({
    sf::st_read(parseFilePaths(volumes,
                               input$nonbreed_poly_pth)$datapath)
  })

  assess_poly <- reactive({
    sf::st_read(parseFilePaths(volumes,
                               input$assess_poly_pth)$datapath)
  })

  hs_rast <- reactive({
    raster::raster(parseFilePaths(volumes,
                                  input$hs_rast_pth)$datapath)
  })

  # Show map when load button clicked (seems like parent expressions don't run
  # until child is visible)
  observeEvent(input$loadSpatial, {
    shinyjs::show("range_map")
    shinyjs::show("spat_res")
  })

  # Make map
  output$range_map <- tmap::renderTmap({
    if(input$rast_plot %in% c(names(clim_vars()))){
      rast <- clim_vars()[[input$rast_plot]]
    } else {
      rast <- hs_rast()
    }

    poly <- switch(input$poly_plot,
      nonbreed_poly = nonbreed_poly(),
      assess_poly = assess_poly(),
      tundra = clim_vars()[["tundra"]]
    )

    tm_shape(rast)+
      tm_raster()+
      tm_shape(range_poly())+
      tm_borders()+
      tm_shape(poly)+
      tm_borders(col = "red")
  })


  # run spatial calculations
  spat_res <- reactive({
    run_CCVI_funs(species_nm = input$species_name,
                  scale_nm = input$geo_location,
                  range_poly = range_poly(),
                  non_breed_poly = nonbreed_poly(),
                  scale_poly = assess_poly(),
                  hs_rast = hs_rast(),
                  clim_vars_lst = clim_vars(),
                  eer_pkg = TRUE)
  })

  # When next button is clicked move to next panel
  observeEvent(input$next2, {
    updateTabsetPanel(session, "tabset",
                      selected = "Vulnerability Questions"
    )
  })

  # # Do spatial analysis Should spatial analysis results be displayed interactively?
  # output$spat_res <- DT::renderDataTable({
  #   spat_res()
  # })

  # Vulnerability Qs #===============
  # Show guidelines with additional info for each section
  observeEvent(input$guideB, {
    showModal(modalDialog(
      title = "Section B Guidelines",
      includeHTML("C:/Users/endicotts/Documents/Definitions and Guidelines for Scoring Risk Factors.html")
    ))
  })

  observeEvent(input$guideC, {
    showModal(modalDialog(
      title = "Section C Guidelines",
      includeHTML("C:/Users/endicotts/Documents/Definitions and Guidelines for Scoring Risk Factors.html")
    ))
  })

  # Calculate Index value #================================

  # Gather all the form inputs
  vuln_df <- eventReactive(input$submitVuln, {
    vuln_qs <- stringr::str_subset(names(input), "[B,C,D]\\d.*")
    data <- purrr::map_df(vuln_qs, ~getMultValues(input[[.x]], .x))
    as_tibble(data)
  })

  output$formData <- renderPrint(vuln_df())

  index_res <- reactive({
    z_df <- data.frame(Code = c("Z2", "Z3"),
                      Value1 = as.numeric(c(input$cave, input$mig)))

    vuln_df <- bind_rows(vuln_df(), z_df) %>%
      mutate(Species = input$species_name)

    index <- calc_vulnerability(spat_res(), vuln_df)
    index
  })

  output$index <- renderText(index_res()$index)
  output$conf_index <- renderText(index_res()$conf_index)
  output$conf_graph <- renderPlot({
    ggplot2::quickplot(x= index, y = frequency, data = index_res()$index_conf,
                       geom = "col")+
     ggplot2::theme_classic()
  })
}

shinyApp(ui, server)
