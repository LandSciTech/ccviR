# based on example app: https://github.com/daattali/shiny-server/blob/master/mimic-google-form/app.R
# and blog post explaining it: https://deanattali.com/2015/06/14/mimicking-google-form-shiny/
library(shiny)
library(shinyFiles)
library(tmap)
library(tidyr)
library(ggplot2)
library(dplyr)
#devtools::load_all()
library(ccviR)

# which fields are mandatory
fieldsMandatory1 <- c("assessor_name", "geo_location", "tax_grp", "species_name")

fieldsMandatory2 <- c("clim_var_dir", "range_poly_pth", "assess_poly_pth")

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
  if(is.null(x)){
    x <- -1
  }
  x <- as.numeric(x)

  df <- data.frame(Code = nm, Value1 = x[1], Value2 = x[2], Value3 = x[3],
                   Value4 = x[4], stringsAsFactors = FALSE)

}

# Name of input data layers for mapping
rast_nms <- list(Temperature = "mat",
                 Precipitation = "map",
                 Moisture = "cmd",
                 `Climate change exposure index` = "ccei",
                 `Historical thermal niche` = "htn",
                 `Habitat suitability` = "hs_rast")

poly_nms <- list(`Assessment area`= "assess_poly",
                 `Non-breeding range` = "nonbreed_poly",
                 `Physiological thermal niche` = "ptn")

# function to make maps
make_map <- function(poly1, rast = NULL, poly2 = NULL,
                     poly1_nm = "Range", poly2_nm = NULL,
                     rast_nm = NULL, rast_style = "cat"){

  # tried adding a line break to legend but doesn't work in interactive map
  poly2_nm <- names(poly_nms)[which(poly_nms == poly2_nm)]
  rast_nm <- names(rast_nms)[which(rast_nms == rast_nm)]

  if(is.null(poly2)){
    out <-  tm_shape(rast)+
      tm_raster(title = rast_nm, style = rast_style)+
      tm_shape(poly1)+
      tm_borders()+
      tm_add_legend("fill", labels = c(poly1_nm),
                    col = c("black"))
  } else if(is.null(rast)){
    out <- tm_shape(poly1)+
      tm_borders()+
      tm_shape(poly2)+
      tm_borders(col = "red")+
      tm_add_legend("fill", labels = c(poly1_nm, poly2_nm),
                    col = c("black", "red"))
  } else {
    out <-  tm_shape(rast)+
      tm_raster(title = rast_nm)+
      tm_shape(poly1)+
      tm_borders()+
      tm_shape(poly2)+
      tm_borders(col = "red")+
      tm_add_legend("fill", labels = c(poly1_nm, poly2_nm),
                    col = c("black", "red"))
  }
  return(out)
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
        fluidRow(
          column(12,
                 div(
                   id = "spatial",
                   h3("Spatial data analysis"),
                   h4("Required spatial datasets"),
                   labelMandatory(strong("Folder location of climate data:")),
                   shinyDirButton("clim_var_dir", "Choose a folder",
                                  "Folder location of climate data"),
                   verbatimTextOutput("clim_var_dir", placeholder = TRUE),
                   br(),
                   labelMandatory(strong("Range polygon shapefile:")),
                   shinyFilesButton("range_poly_pth", "Choose file",
                                    "Range polygon shapefile", multiple = FALSE),
                   verbatimTextOutput("range_poly_pth", placeholder = TRUE),
                   br(),
                   labelMandatory(strong("Assessment area polygon shapefile")),
                   shinyFilesButton("assess_poly_pth", "Choose file",
                                    "Assessment area polygon shapefile",
                                    multiple = FALSE),
                   verbatimTextOutput("assess_poly_pth", placeholder = TRUE),
                   br(),
                   strong("Non-breeding Range polygon shapefile"),
                   shinyFilesButton("nonbreed_poly_pth", "Choose file",
                                    "Non-breeding Range polygon shapefile",
                                    multiple = FALSE),
                   verbatimTextOutput("nonbreed_poly_pth", placeholder = TRUE),
                   br(),
                   strong("Habitat suitability raster file"),
                   shinyFilesButton("hs_rast_pth", "Choose file",
                                    "Habitat suitability raster file", multiple = FALSE),
                   verbatimTextOutput("hs_rast_pth", placeholder = TRUE),
                   br(),
                   strong("Click Load to explore map or Next to proceed"),
                   br(),
                   actionButton("loadSpatial", "Load", class = "btn-primary")




                 )
          )
        ),
        fluidRow(
          column(
            6,
            div(
              id = "texp_map",
              h3("Temperature exposure"),
              tmap::tmapOutput("texp_map"),
              tableOutput("texp_tbl")
            )
          ),
          column(
              6,
              div(
                id = "cmd_map",
                h3("Moisture exposure"),
                tmap::tmapOutput("cmd_map"),
                tableOutput("cmd_tbl")
              )
          )
        ),
        fluidRow(
          actionButton("next2", "Next", class = "btn-primary"),
          br(), br()
        )
      ),
      # Section B questions #=================
      tabPanel(
        "Vulnerability Questions",
        fluidRow(
          column(12,
                 div(id = "secB",
                     br(), br(),
                     h4("Allow override of spatial analysis results?"),
                     checkboxInput("override_spatial", "If checked questions below that are answered by the spatial analysis will be enabled and if they are filled in they will supersede the answer given by the spatial analysis",
                                   width = "100%"),

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

                     checkboxGroupInput("B3", "  3) Predicted impact of land use changes resulting from human responses to climate change",
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
                     shinyjs::disabled(
                       div(
                         id = "override1",
                         checkboxGroupInput("C2ai", HTML("i) historical thermal niche.<font color=\"#FF0000\"><b> Check override spatial analysis to enable.</b></font>"),
                                            choiceNames = valueNms,
                                            choiceValues = valueOpts,
                                            inline = TRUE),

                         checkboxGroupInput("C2aii", HTML("ii) physiological thermal niche. <font color=\"#FF0000\"><b>Check override spatial analysis to enable.</b></font>"),
                                            choiceNames = valueNms,
                                            choiceValues = valueOpts,
                                            inline = TRUE)
                       )
                     ),

                     strong("2b) Predicted sensitivity to changes in precipitation, hydrology, or moisture regime:"),
                     shinyjs::disabled(
                       div(
                         id = "override2",
                         checkboxGroupInput("C2bi", HTML("i)historical hydrological niche.  <font color=\"#FF0000\"><b>Check override spatial analysis to enable.</b></font>"),
                                            choiceNames = valueNms,
                                            choiceValues = valueOpts,
                                            inline = TRUE)
                       )
                     ),
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
                     shinyjs::hidden(
                       div(
                         id = "animal_only",
                         checkboxGroupInput("C4b", "4b) Dietary versatility (animals only).",
                                            choiceNames = valueNms[2:4],
                                            choiceValues = valueOpts[2:4],
                                            inline = TRUE)
                       )
                     ),
                     shinyjs::hidden(
                       div(
                         id = "plant_only",
                         checkboxGroupInput("C4c", "4c) Pollinator versatility (plants only).",
                                            choiceNames = valueNms[2:4],
                                            choiceValues = valueOpts[2:4],
                                            inline = TRUE)
                       )
                     ),
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
                     checkboxGroupInput("C4g", "4g) Forms part of an interspecific interaction not covered by 4a-f.",
                                        choiceNames = valueNms[2:4],
                                        choiceValues = valueOpts[2:4],
                                        inline = TRUE),

                     checkboxGroupInput("C5a", "5a) Measured genetic variation.",
                                        choiceNames = valueNms[2:4],
                                        choiceValues = valueOpts[2:4],
                                        inline = TRUE),
                     conditionalPanel(
                       "input.C5a == ''",
                       checkboxGroupInput("C5b", "5b) Occurrence of bottlenecks in recent evolutionary history (use only if 5a is unknown).",
                                          choiceNames = valueNms[2:4],
                                          choiceValues = valueOpts[2:4],
                                          inline = TRUE),

                     ),
                     conditionalPanel(
                       "input.C5a == '' && input.C5b == ''",
                       shinyjs::hidden(
                         div(
                           id = "plant_only2",
                           checkboxGroupInput("C5b", "5c) Reproductive system (plants only; use only if C5a and C5b are unknown).",
                                              choiceNames = valueNms[2:4],
                                              choiceValues = valueOpts[2:4],
                                              inline = TRUE)
                         )
                       )
                     ),

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
                     shinyjs::disabled(
                       div(
                         id = "override3",
                         checkboxGroupInput("D2", HTML("2) Modeled future (2050) change in population or range size. <font color=\"#FF0000\"><b>Check override spatial analysis to enable.</b></font>"),
                                            choiceNames = valueNms,
                                            choiceValues = valueOpts,
                                            inline = TRUE),
                         checkboxGroupInput("D3", HTML("3) Overlap of modeled future (2050) range with current range. <font color=\"#FF0000\"><b>Check override spatial analysis to enable.</b></font>"),
                                            choiceNames = valueNms,
                                            choiceValues = valueOpts,
                                            inline = TRUE)
                       )
                     ),
                     checkboxGroupInput("D4", "4) Occurrence of protected areas in modeled future (2050) distribution.",
                                        choiceNames = valueNms[2:4],
                                        choiceValues = valueOpts[2:4],
                                        inline = TRUE),
                     actionButton("submitVuln", "Next", class = "btn-primary")
                 )
          )
        )
      ),
      # Spatial Vulnerability Questions #================================
        tabPanel(
          "Spatial Vulnerability Questions",
          fluidRow(
            column(12,
                   div(
                     id = "C2ai",
                     h4("2a) Predicted sensitivity to changes in temperature:"),
                     br(),
                     strong("i) Historical hydrological niche."),
                     br(),br(),
                     tmapOutput("C2ai_map", width = "50%"),
                     tableOutput("C2ai_tbl"),
                     uiOutput("C2ai_box")
                   )
            )
          )
      ),
      # Results #===================================
      tabPanel(
        "Results",
        column(6,
               div(
                 id = "formData",
                 h3("Results"),
                 strong("Climate Change Vulnerability Index: "),
                 br(),
                 h4(shinycssloaders::withSpinner(htmlOutput("index"))),
                 br(), br(),
                 tableOutput("n_factors"),
                 strong("Confidence in index: "),
                 textOutput("conf_index"),
                 plotOutput("conf_graph", width = 300, height = 200),
                 br(), br(),
                 downloadButton("downloadData", "Download results as csv"),
                 br(), br(),
                 actionButton("restart", "Assess another species", class = "btn-primary")
               )),
        column(6,
               div(id = "indplt",
                   br(), br(), br(), br(), br(),
                   plotOutput("ind_score_plt"),
                   textOutput("slr"),
                   plotOutput("q_score_plt"),
                   verbatimTextOutput("test_vulnQ"),
                   tableOutput("vuln_df_tbl")))
      )
    )
)


  # Server #========================
server <- function(input, output, session) {

  # start up Note this time out is because when I disconnected from VPN it
  # made the getVolumes function hang forever because it was looking for
  # drives that were no longer connected. Now it will give an error
  R.utils::withTimeout({
    volumes <- c(wd = getShinyOption("file_dir"),
                 Home = fs::path_home(),
                 getVolumes()())
  }, timeout = 10, onTimeout = "error")


  # Species Info #=================
  # Enable the Submit button when all mandatory fields are filled out
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory1,
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

  # Spatial Analysis #===============
  # Enable the Submit button when all mandatory fields are filled out
  observe({
    mandatoryFilled2 <-
      vapply(fieldsMandatory2,
             function(x) {
               isTruthy(input[[x]])
             },
             logical(1))
    mandatoryFilled2 <- all(mandatoryFilled2)

    shinyjs::toggleState(id = "loadSpatial", condition = mandatoryFilled2)
    shinyjs::toggleState(id = "next2", condition = mandatoryFilled2)
  })

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

    clim_vars <- get_clim_vars(root_pth)

  })

  range_poly <- reactive({
    sf::st_read(parseFilePaths(volumes,
                               input$range_poly_pth)$datapath,
                agr = "constant", quiet = TRUE)
  })

  nonbreed_poly <- reactive({
    pth <- parseFilePaths(volumes,
                          input$nonbreed_poly_pth)$datapath
    if(!isTruthy(pth)){
      return(NULL)
    }
    sf::st_read(pth, agr = "constant", quiet = TRUE)
  })

  assess_poly <- reactive({
    sf::st_read(parseFilePaths(volumes,
                               input$assess_poly_pth)$datapath,
                agr = "constant", quiet = TRUE)
  })

  hs_rast <- reactive({
    pth <- parseFilePaths(volumes,
                          input$hs_rast_pth)$datapath
    if(!isTruthy(pth)){
      return(NULL)
    }

    raster::raster(pth)
  })

  # run spatial calculations
  spat_res <- eventReactive(input$loadSpatial,{
    run_spatial(range_poly = range_poly(),
                non_breed_poly = nonbreed_poly(),
                scale_poly = assess_poly(),
                hs_rast = hs_rast(),
                clim_vars_lst = clim_vars(),
                eer_pkg = TRUE)
  })


  # Make maps
  output$texp_map <- tmap::renderTmap({
    req(input$loadSpatial)

    make_map(range_poly(), clim_vars()$mat, rast_nm = "mat")
  })

  output$cmd_map <- tmap::renderTmap({
    req(input$loadSpatial)

    make_map(range_poly(), clim_vars()$cmd, rast_nm = "cmd")
  })

  output$texp_tbl <- renderTable({
    exp_df <-  spat_res() %>%
      mutate(temp_exp = case_when(
        MAT_1 > 50 ~ 2.4,
        sum(MAT_1, MAT_2, na.rm = TRUE) >= 75 ~ 2,
        sum(MAT_1, MAT_2, MAT_3, na.rm = TRUE) >= 60 ~ 1.6,
        sum(MAT_1, MAT_2, MAT_3, MAT_4, na.rm = TRUE) >= 40 ~ 1.2,
        sum(MAT_1, MAT_2, MAT_3, MAT_4, MAT_5, na.rm = TRUE) >= 20 ~ 0.8,
        TRUE ~ 0.4
      ),
      temp_exp_cave = temp_exp / ifelse(input$cave == 1, 3, 1)) %>%
      select(contains("MAT"), temp_exp_cave) %>%
      rename_at(vars(contains("MAT")),
                ~stringr::str_replace(.x, "MAT_", "Class ")) %>%
      rename(`Exposure Multiplier` = temp_exp_cave) %>%
      pivot_longer(cols = contains("Class"),
                   names_to = "Change Class", values_to = "Proportion of Range") %>%
      transmute(`Change Class`, `Proportion of Range`,
                `Exposure Multiplier` = c(as.character(`Exposure Multiplier`[1]),
                                          rep("", n() - 1)))

  })

  output$cmd_tbl <- renderTable({
    exp_df <-  spat_res() %>%
      mutate(moist_exp = case_when(
        CMD_1 >= 80 ~ 2,
        sum(CMD_1, CMD_2, na.rm = TRUE) >= 64 ~ 1.67,
        sum(CMD_1, CMD_2, CMD_3, na.rm = TRUE) >= 48 ~ 1.33,
        sum(CMD_1, CMD_2, CMD_3, CMD_4, na.rm = TRUE) >= 32 ~ 1,
        sum(CMD_1, CMD_2, CMD_3, CMD_4, CMD_5, na.rm = TRUE) >= 16 ~ 0.67,
        TRUE ~ 0.33
      ),
      moist_exp_cave = moist_exp / ifelse(input$cave == 1, 3, 1)) %>%
      select(contains("CMD"), moist_exp_cave) %>%
      rename_at(vars(contains("CMD")),
                ~stringr::str_replace(.x, "CMD_", "Class ")) %>%
      rename(`Exposure Multiplier` = moist_exp_cave) %>%
      pivot_longer(cols = contains("Class"),
                   names_to = "Change Class", values_to = "Proportion of Range") %>%
      transmute(`Change Class`, `Proportion of Range`,
                `Exposure Multiplier` = c(as.character(`Exposure Multiplier`[1]),
                                          rep("", n() - 1)))

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

  observe({
    shinyjs::toggleState(id = "override1",
                         condition = input$override_spatial)
    shinyjs::toggleState(id = "override2",
                         condition = input$override_spatial)
    shinyjs::toggleState(id = "override3",
                         condition = input$override_spatial)
    })

  # When next button is clicked move to next panel
  observeEvent(input$submitVuln, {
    updateTabsetPanel(session, "tabset",
                      selected = "Spatial Vulnerabiliy Questions")
  })

  # Spatial Vulerability Questions #========================
  output$C2ai_map <- renderTmap({
    req(input$submitVuln)

    make_map(range_poly(), rast = clim_vars()$htn, rast_nm = "htn")
  })

  output$C2ai_tbl <- renderTable({
    exp_df <-  spat_res() %>%
      select(contains("HTN")) %>%
      rename_at(vars(contains("HTN")),
                ~stringr::str_replace(.x, "HTN_", "Class ")) %>%
      pivot_longer(cols = contains("Class"),
                   names_to = "Change Class", values_to = "Proportion of Range") %>%
      transmute(`Change Class`, `Proportion of Range`)
  })

  output$C2ai_box <- renderUI({
    box_val <- spat_res() %>%
      mutate(C2ai = case_when(HTN_4 > 10 ~ 0,
                              HTN_3 > 10 ~ 1,
                              HTN_2 > 10 ~ 2,
                              HTN_1 > 10 ~ 3,
                              is.na(HTN_1) ~ NA_real_)) %>%
      pull(C2ai)

    checkboxGroupInput("C2ai", HTML("Calculated effect on vulnerability. <font color=\"#FF0000\"><b> Editing this response will override the results of the spatial analysis.</b></font>"),
                       choiceNames = valueNms,
                       choiceValues = valueOpts,
                       selected = box_val,
                       inline = TRUE)
  })


  # Calculate Index value #================================

  # Gather all the form inputs
  vuln_df <- eventReactive(input$submitVuln, {
    vuln_qs <- stringr::str_subset(names(input), "[B,C,D]\\d.*")
    data <- purrr::map_df(vuln_qs, ~getMultValues(input[[.x]], .x))
    as_tibble(data)
  })

  # Useful for testing
  # output$test_vulnQ <- renderPrint({
  #   print(input[["B1"]] %>% str())
  #   x <- ifelse(is.null(input[["B1"]]), -1, input[["B1"]])
  #   print(x)
  #   x <- as.numeric(x)
  #   print(x)
  #
  #   df <- data.frame(Code = "B1", Value1 = x[1], Value2 = x[2], Value3 = x[3],
  #                    Value4 = x[4], stringsAsFactors = FALSE)
  #   print(df)
  #   })
  #
  # output$vuln_df_tbl <- renderTable(index_res()$vuln_df %>% arrange(Code))

  index_res <- reactive({
    z_df <- data.frame(Code = c("Z2", "Z3"),
                      Value1 = as.numeric(c(input$cave, input$mig)))

    vuln_df <- bind_rows(vuln_df(), z_df) %>%
      mutate(Species = input$species_name)

    index <- calc_vulnerability(spat_res(), vuln_df)
    index
  })

  output$index <- renderText({
    ind <- index_res()$index
    col <- case_when(ind == "IE" ~ "grey",
                     ind == "EV" ~ "red",
                     ind == "HV" ~ "orange",
                     ind == "MV" ~ "yellow",
                     ind == "LV" ~ "green",
                     TRUE ~ "grey")
    ind <- case_when(ind == "IE" ~ "Insufficient Evidence",
              ind == "EV" ~ "Extremely Vulnerable",
              ind == "HV" ~ "Highly Vulnerable",
              ind == "MV" ~ "Moderately Vulnerable",
              ind == "LV" ~ "Less Vulnerable",
              TRUE ~ "Insufficient Evidence")

    paste("<font color=", col, "><b>", ind, "</b></font>")

    })
  output$n_factors <- renderTable({
    tibble(Section = c("Section B", "Section C", "Section D"),
           `Number of factors with data` = c(index_res()$n_b_factors,
                                             index_res()$n_c_factors,
                                             index_res()$n_d_factors),
           `Number of factors` = c(4L, 16L, 4L))
  })

  output$slr <- renderText({
    if(!index_res()$slr_vuln){
      return(NULL)
    }
    paste0("The index value for this species was increased to ",
           "'Extremely Vulnerable' because it is vulnerable to rising ",
           "sea levels and has significant dispersal barriers")
  })

  output$ind_score_plt <- renderPlot({
    b_c_score <- case_when(index_res()$n_b_factors < 3 ~ NA_real_,
                           index_res()$n_c_factors < 10 ~ NA_real_,
                           TRUE ~ index_res()$b_c_score)

    d_score <- case_when(index_res()$n_d_factors < 1 ~ 0,
                         TRUE ~ index_res()$d_score)

    # if b_c is IE no plot if d is IE set to 0 but still plot
    if(is.na(b_c_score)){
      return(NULL)
    } else {
      plot_score_index(b_c_score, index_res()$d_score)
    }
  })

  output$conf_index <- renderText(index_res()$conf_index)
  output$conf_graph <- renderPlot({
    ggplot2::quickplot(x = factor(index, levels = c( "EV", "HV", "MV", "LV", "IE")),
                       y = frequency,
                       data = index_res()$index_conf,
                       geom = "col", xlab = "Index", ylab = "Proportion of Runs",
                       main = "Monte Carlo Simulation Results",
                       ylim = c(NA, 1))+
     ggplot2::theme_classic()
  })

  # TODO: make this prettier and uncomment
  # output$q_score_plt <- renderPlot({
  #   plot_q_score(index_res()$vuln_df)
  # })

  out_data <- reactive({
    vuln_df <- index_res()$vuln_df %>%
      select(Code, contains("Value")) %>%
      filter(!Code %in% c("Z2", "Z3")) %>%
      arrange(Code) %>%
      mutate_all(as.character) %>%
      tidyr::unite(Value, Value1:Value4, na.rm = TRUE, sep = ", ") %>%
      pivot_wider(names_from = "Code", values_from = "Value")

    spat_df <- spat_res()

    conf_df <- index_res()$index_conf %>%
      mutate(index = paste0("MC_freq_", index)) %>%
      pivot_wider(names_from = "index", values_from = "frequency")

    data.frame(species_name = input$species_name,
               common_name = input$common_name,
               geo_location = input$geo_location,
               assessor = input$assessor_name,
               taxonomic_group = input$tax_grp,
               migratory = input$mig,
               cave_grnd_water = input$cave,
               CCVI_index = index_res()$index,
               CCVI_conf_index = index_res()$conf_index,
               mig_exposure = index_res()$mig_exp,
               b_c_score = index_res()$b_c_score,
               d_score = index_res()$d_score) %>%
      bind_cols(conf_df, spat_df, vuln_df)
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("CCVI_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(out_data(), file, row.names = FALSE)
    }
  )

  observeEvent(input$restart,{
    shinyjs::refresh()
  })

}

shinyApp(ui, server,
         options = list(launch.browser = getShinyOption("launch.browser"),
                        port = getShinyOption("port")))
