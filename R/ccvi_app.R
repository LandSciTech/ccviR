# based on example app: https://github.com/daattali/shiny-server/blob/master/mimic-google-form/app.R
# and blog post explaining it: https://deanattali.com/2015/06/14/mimicking-google-form-shiny/



#' Title
#'
#' @param ...
#'
#' @return
#'
#' @import shiny
#' @import dplyr
#' @import sf
#' @import shinyFiles
#' @importFrom raster raster crs
#' @importFrom tmap tmap_leaflet
#' @export
#'
#' @examples
ccvi_app <- function(...){
  # which fields are mandatory
  fieldsMandatory1 <- c("assessor_name", "geo_location", "tax_grp", "species_name")

  fieldsMandatory2 <- c("clim_var_dir", "range_poly_pth", "assess_poly_pth")

  # File path ids to use with file choose
  filePathIds <- c("range_poly_pth", "nonbreed_poly_pth", "assess_poly_pth",
                   "hs_rast_pth")

  # Input options
  valueNms <- c("Greatly increase", "Increase", "Somewhat increase", "Neutral")
  valueOpts <- c(3, 2, 1, 0)

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

    tabsetPanel(
      id = "tabset",
      # Introduction #===============
      tabPanel(
        "Welcome",
        tabsetPanel(
          id = "welcome", type = "hidden",
          tabPanelBody("instructions",
            fluidPage(
              h2("Welcome"),
              p("This app provides a new interface for the Climate Change Vulnerability Index created by ",
                a("NatureServe", href = "https://www.natureserve.org/conservation-tools/climate-change-vulnerability-index"),
                "that automates the spatial analysis needed to inform the index"),
              h3("Preparing to use the app"),

              p(strong("Step 0: "),"The first time you use the app ",
                "you will need to prepare the climate data used in the app."),
              actionButton("prep_data", "Prepare Climate Data", class = "btn-primary"),
              br(), br(),
              p(strong("Step 1: "), "Acquire species-specific spatial datasets:",
                tags$ul(
                  tags$li(labelMandatory("Species range polygon")),
                  tags$li(labelMandatory("Assessment area polygon")),
                  tags$li("Non-breeding range polygon"),
                  tags$li("Projected habitat change raster classified as",
                          " 1 = lost, 2-6 = maintained and 7 = gained"),
                  tags$li("Physiological thermal niche (PTN) polygon. ",
                          "PTN polygon should include cool or cold environments ",
                          "that the species occupies that may be lost or reduced ",
                          "in the assessment area as a result of climate change."))),
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
              actionButton("start", "Start", class = "btn-primary")
            ),
          ),
          tabPanelBody("data_prep",
            data_prep_ui("data_prep_mod"),
            shinycssloaders::withSpinner(verbatimTextOutput("data_prep_msg",
                                                            placeholder = TRUE)),
            actionButton("data_done", "Finished", class = "btn-primary")

          )
        )

      ),
      # Species Info #===============
      tabPanel(
        "Species Information",
        column(
          width = 12,
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
        )
      ),
      # Spatial Analysis #============
      tabPanel(
        "Spatial Data Analysis",
        fluidRow(
          column(
            12,
            div(
              id = "spatial",
              h3("Spatial data analysis"),
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
              strong("Click Load to begin spatial analysis"),
              br(),
              actionButton("loadSpatial", "Load", class = "btn-primary")
            )
          )
        ),
        fluidRow(
          column(
            6,
            div(
              id = "texp_map_div",
              h3("Temperature exposure"),
              shinycssloaders::withSpinner(tmap::tmapOutput("texp_map")),
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
          column(
            12,
            actionButton("next2", "Next", class = "btn-primary"),
            br(), br()
          )
        )
      ),
      # Section B questions #=================
      tabPanel(
        "Vulnerability Questions",
        fluidRow(
          column(
            12,
            h3("Vulnerability Questions"),
            div(
              id = "secB",
              h4("Section B: Indirect Exposure to Climate Change"),
              h4("Evaluate for specific geographical area under consideration"),
              h5("Factors that influence vulnerability"),
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
          column(
            12,
            div(
              id = "secC",
              h4("Section C: Sensitivity and Adaptive Capacity"),
              actionButton("guideC", "Show guidelines"),
              checkboxGroupInput("C1", "1) Dispersal and movements",
                                 choiceNames = valueNms,
                                 choiceValues = valueOpts,
                                 inline = TRUE),

              strong("2b) Predicted sensitivity to changes in precipitation, hydrology, or moisture regime:"),

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
          column(
            12,
            div(
              id = "secD",
              h4("Section D: Documented or Modeled Response to Climate Change"),
              h5("(Optional; May apply across the range of a species)"),
              actionButton("guideD", "Show guidelines"),

              checkboxGroupInput("D1", "1) Documented response to recent climate change. ",
                                 choiceNames = valueNms,
                                 choiceValues = valueOpts,
                                 inline = TRUE),

              checkboxGroupInput("D4", "4) Occurrence of protected areas in modeled future (2050) distribution.",
                                 choiceNames = valueNms[2:4],
                                 choiceValues = valueOpts[2:4],
                                 inline = TRUE),
              actionButton("nextVuln", "Next", class = "btn-primary"),
              br(), br()
            )
          )
        )
      ),
      # Spatial Vulnerability Questions #================================
      tabPanel(
        "Spatial Vulnerability Questions",
        fluidRow(
          column(
            12,
            h3("Spatial Vulnerability Questions"),
            h4("Section C: Sensitivity and Adaptive Capacity"),
            br(),
            div(
              id = "C2ai",
              h4("Predicted sensitivity to temperature and moisture changes:"),
              strong("2a) i) Historical thermal niche."),
              br(),br(),
              div(id = "missing_htn",
                  HTML("<font color=\"#FF0000\"><b>Data set not provided.</b></font> <br>Answer the questions below based on expert knowledge or leave blank for unknown."),
                  br(),
                  br()),
              shinycssloaders::withSpinner(tmap::tmapOutput("map_C2ai", width = "50%")),
              tableOutput("tbl_C2ai"),
              uiOutput("box_C2ai")
            ),
            div(
              id = "C2aii",
              strong("2a) ii) Physiological thermal niche."),
              br(),br(),
              div(id = "missing_ptn",
                  HTML("<font color=\"#FF0000\"><b>Data set not provided.</b></font> <br>Answer the questions below based on expert knowledge or leave blank for unknown."),
                  br(),
                  br()),
              tmap::tmapOutput("map_C2aii", width = "50%"),
              tableOutput("tbl_C2aii"),
              uiOutput("box_C2aii")
            ),
            div(
              id = "C2bi",
              strong("2b) i) Historical hydrological niche."),
              br(),br(),
              div(id = "missing_map",
                  HTML("<font color=\"#FF0000\"><b>Data set not provided.</b></font> <br>Answer the questions below based on expert knowledge or leave blank for unknown."),
                  br(),
                  br()),
              tmap::tmapOutput("map_C2bi", width = "50%"),
              tableOutput("tbl_C2bi"),
              uiOutput("box_C2bi")
            ),
            h4("Section D: Documented or Modeled Response to Climate Change"),
            br(),
            div(
              id = "D2_3",
              h4("Modelled change in habitat suitability"),
              br(),
              div(id = "missing_hs",
                  HTML("<font color=\"#FF0000\"><b>Data set not provided.</b></font> <br>Answer the questions below based on expert knowledge or leave blank for unknown."),
                  br(),
                  br()),
              tmap::tmapOutput("map_D2_3", width = "50%"),
              tableOutput("tbl_D2_3"),
              strong("2) Modeled future (2050) change in population or range size."),
              uiOutput("box_D2"),
              strong("3) Overlap of modeled future (2050) range with current range"),
              uiOutput("box_D3")
            ),
            actionButton("submitSpatVuln", "Submit", class = "btn-primary")
          )
        )
      ),
      # Results #===================================
      tabPanel(
        "Results",
        column(
          6,
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
            actionButton("restart", "Assess another species",
                         class = "btn-primary")
          )
        ),
        column(
          6,
          div(
            id = "indplt",
            br(), br(), br(), br(), br(),
            plotOutput("ind_score_plt"),
            textOutput("slr"),
            plotly::plotlyOutput("q_score_plt"),
            verbatimTextOutput("test_vulnQ"),
            tableOutput("vuln_df_tbl")
          )
        )
      )
    )
  )


  # Server #========================
  server <- function(input, output, session) {

    prepped_data <- data_prep_server("data_prep_mod")

    output$data_prep_msg <- renderText(prepped_data())

    observeEvent(input$prep_data, {
      updateTabsetPanel(session, "welcome", selected = "data_prep")
    })

    observeEvent(input$data_done, {
      updateTabsetPanel(session, "welcome", selected = "instructions")
    })

    observeEvent(input$start, {
      updateTabsetPanel(session, "tabset",
                        selected = "Species Information"
      )
      shinyjs::runjs("window.scrollTo(0, 0)")
    })

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
    purrr::map(filePathIds, shinyFileChoose, root = volumes, input = input, filetypes = c("shp", "tif", "asc", "nc", "grd", "bil"))

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

      check_trim(raster::raster(pth))
    })

    # run spatial calculations
    spat_res <- reactive({
      req(input$loadSpatial)
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
        tidyr::pivot_longer(cols = contains("Class"),
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
        tidyr::pivot_longer(cols = contains("Class"),
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
      shinyjs::runjs("window.scrollTo(0, 0)")
    })

    # Vulnerability Qs #===============
    # Show guidelines with additional info for each section
    observeEvent(input$guideB, {
      showModal(modalDialog(
        title = "Section B Guidelines",
        HTML("<div>

<p>The NatureServe Climate Change Vulnerability Index release
3.02   </p>

<p>  </p>

<p>Definitions and Guidelines for Scoring Risk Factors - Section
B   </p>

<p> Response required for at least 3 factors.  </p>

<p>  </p>

<p>B. Indirect Exposure to Climate Change  </p>

<p>  </p>

<p> 1) Exposure to Sea Level Rise  </p>

<p> NOTES: This factor comes into play only in
the case that all or a portion of the range within the assessment area may be
subject to the effects of a 0.5-1 m or greater sea level rise and the
consequent influence of storm surges and intrusion of salt water. Most climate
model scenarios predict at least a 0.5 m sea level rise. Because projected sea
level rise (0.5-2 m by 2100) is great compared to historical sea level changes,
the negative impact on habitats for most affected species is expected to be
high.
<p> TOOLS: To visualize potential sea level rise
in coastal areas of the U.S., see http://coast.noaa.gov/digitalcoast/tools/slr.  </p>

<p> Greatly
Increase Vulnerability: &gt;90% of range occurs in area subject to sea
level rise (on low-lying island(s) or in coastal zone).   </p>

<p> Increase
Vulnerability: 50-90% of range occurs in area subject to sea level rise (on
low-lying island(s) or in coastal zone).  </p>

<p> Somewhat
Increase Vulnerability: 10-49% of range occurs in area subject to
sea level rise (on low-lying island(s) or in coastal zone).  </p>

<p> Neutral: &lt;10%
of range occur in area subject to sea level rise (on low-lying island(s) or in
coastal zone). Includes inland areas not subject to sea level rise. Also,
species that occur in an intertidal habitat that is expected to increase in
extent with a rising sea level.  </p>

<p>  </p>

<p> 2) Distribution Relative to Barriers  </p>

<p> NOTES: This factor assesses the degree to
which natural (e.g., topographic, geographic, ecological) or anthropogenic
barriers limit a species' ability to shift its range in response to climate
change. Barriers are defined here as features or areas that completely or
almost completely prevent movement or dispersal of the species (currently and
for the foreseeable future). Species for which barriers would inhibit
distributional shifts with climate change-caused shifts in climate envelopes
likely are more vulnerable to climate change than are species whose movements
are not affected by barriers. Barriers must be identified for each species (but
often are the same for a group of closely related species). Natural and
anthropogenic barriers are defined for many species and taxonomic groups in
NatureServe's Element Occurrence Specifications (viewable in the Population/Occurrence
Delineation section of species accounts on Natureserve Explorer,
http://www.natureserve.org/explorer), but usually these readily can be
determined by considering a species' basic movement capacity and ecological
tolerances.  </p>

<p> The distinction between a
barrier and unsuitable habitat sometimes may be unclear; in these cases assume
the feature or area is unsuitable habitat (habitat through which the species
can disperse or move but that does not support reproduction or long-term
survival) and score the species here and/or in factor C1 as appropriate. Note
that caves are considered under factor C3: Restriction to Uncommon
Landscape/Geological Features, and not here where the focus is on barriers that
affect the wide array of nonsubterranean species.  </p>

<p> A) NATURAL BARRIERS: Examples
of features that may function as natural barriers for various species: upland
habitat (i.e., absence of aquatic stream, lake, or pond habitat) is a barrier
for fishes (but not for semiaquatic or amphibious species that may occupy the
same body of water); high mountain ranges (especially those that extend
west-east) are a barrier for many lowland plants and nonvolant lowland animals;
warm lowlands are a barrier for some alpine species such as American pika but
not for elk or American pipit; large expanses of water are barriers for pocket
gophers and many other small terrestrial animals (but not for many volant
species, or for plant species that are dispersed by wide-ranging birds, or for
species that readily swim between land areas if the distance is not too great);
a high waterfall is a barrier for fishes (but not for American dippers or
gartersnakes that occur along the same stream).  </p>

<p> B) ANTHROPOGENIC BARRIERS:
Examples of features that may function as anthropogenic barriers: large areas
of intensive urban or agricultural development are barriers for many animals
and plants; waters subject to chronic chemical pollution (e.g., acid mine
drainage) can be a barrier for fishes and other strictly aquatic species;
waters subject to thermal pollution (e.g., from power plants) may be a barrier
for some strictly aquatic species but not for others (note thermal alterations
associated with reservoirs often produce unsuitable habitat rather than impose
a barrier); dams without fish passage facilities and improperly installed
culverts can be barriers for fishes and certain other strictly aquatic species;
tortoise-proof fencing may be barrier for small reptiles and certain other
nonvolant animals (but not for most plants, large mammals, or large snakes).  </p>

<p> Note that no barriers exist
for most temperate-zone bird species that simply fly over or around potential
obstructions. Species restricted to habitats that are believed to persist
unchanged in spite of climate change are scored as Neutral (because in these
situations barriers do not contribute to vulnerability even if climate
changes). If a feature or area does not completely or almost completely prevent
dispersal or movement then it is categorized here as unsuitable or suitable
habitat, and the dispersal/movement of individuals across that feature or area
is assessed under factor C1 (Dispersal and Movements). In most cases,
unsuitable habitat is habitat through which propagules or individuals may move
but that does not support reproduction or long-term survival.  </p>

<p> The degree to which a barrier
may affect a species' ability to shift its range in response to climate change
depends in part on the distance of the barrier from the species' current
distribution. Barriers that are separated from a species' range by a long
distance of relatively flat topography can nevertheless affect range shifts
because in gentle terrain relatively small changes in climate can result in
large shifts in the location of a particular climate envelope. If a species
changed its range accordingly (to track a particular climate envelope), it
might encounter barriers that were far from its original range. In contrast, in
landscapes in which climatic conditions change rapidly over small horizontal
distances (e.g., mountainous areas, steep slopes, or other topographically
diverse landscapes) a species' distribution would have to shift a relatively
small distance in order to track a particular climate envelope, so the species
is less likely to encounter distant barriers.   </p>

<p> To count as a barrier for the
purposes of this factor, a feature can be up to 50 km from the species' current
range when measured across areas where climate changes gradually over latitude
or longitude (e.g., relatively flat terrain) and up to 10 km when measured
across areas where climate changes abruptly over latitude or longitude (e.g.,
mountainous or steep terrain). Use 25 km for species that occur in intermediate
topography, such as moderate hill country. These distances apply to both
terrestrial and aquatic species. These distances are derived from Loarie et al.
(2009, Nature 462:1052).  </p>

<p> The following categories and
criteria apply to both natural and anthropogenic barriers, but the two types of
barriers are scored separately. Note that it is illogical for natural and
anthropogenic barriers to both cause greatly increased vulnerability to climate
change for a single species (only one or the other can completely surround a
species' range). If both barriers occur, estimate the relative portions of the
circumference of the range blocked by each and then score accordingly.  </p>

<p> TOOLS: One useful data source for assessing
intensity of land use as a potential anthropogenic barrier in the 48 contiguous
United States is the published maps and downloadable GIS data for
Wildland-Urban Interface of the Silvis Lab (University of Wisconsin-Madison and
the USDA Forest Service North Central Research Station, http://silvis.forest.wisc.edu/maps/wui_main).
Other data sets, such as the Global Land Cover Facility (NASA;
http://glcfapp.glcf.umd.edu/data/) are also acceptable (and offer wider
coverage) but may require more advanced GIS capabilities. Readily available
online sources of satellite imagery also may be useful in assessing
anthropogenic or certain other barriers.  </p>

<p> Greatly Increase Vulnerability: Barriers
completely OR almost completely surround the current distribution such that the
species' range in the assessment area is unlikely to be able to shift
significantly with climate change, or the direction of climate change-caused
shift in the species' favorable climate envelope is fairly well understood and
barriers prevent a range shift in that direction. See Neutral for species in
habitats not vulnerable to climate change.  </p>

<p> Examples
for natural barriers: lowland terrestrial species completely surrounded by high
mountains (or bordered closely and completely on the north side by high
mountains); cool-water stream fishes for which barriers would completely
prevent access to other cool-water areas if the present occupied habitat became
too warm as a result of climate change; most nonvolant species that exist only
on the south side of a very large lake in an area where habitats are expected
to shift northward with foreseeable climate change.  </p>

<p> Examples
for anthropogenic barriers: species limited to small habitats within
intensively developed urban or agricultural landscapes through which the
species cannot pass, A specific example of this category is provided by the
quino checkerspot butterfly (Euphydryas editha quino), a resident of northern
Baja California and southern California; warming climates are forcing this
butterfly northward, but urbanization in San Diego blocks its movement
(Parmesan 1996, Nature 382:765).  </p>

<p> Increase Vulnerability: Barriers
border the current distribution such that climate change-caused distributional
shifts in the assessment area are likely to be greatly but not completely or
almost completely impaired.  </p>

<p> Examples
for natural barriers: certain lowland plant or small mammal species whose
ranges are mostly (50-90%) bordered by high mountains or a large lake.  </p>

<p> Examples
for anthropogenic barriers: most streams inhabited by a fish species have dams
that would prevent access to suitable habitat if the present occupied habitat
became too warm as a result of climate change; intensive urbanization surrounds
75% of the range of a salamander species.  </p>

<p> Somewhat Increase Vulnerability: Barriers
border the current distribution such that climate change-caused distributional
shifts in the assessment area are likely to be significantly but not greatly or
completely impaired.  </p>

<p> Examples
for natural barriers: certain lowland plant or small mammal species whose
ranges are partially but not mostly bordered by high mountains or a large lake.  </p>

<p> Examples
for anthropogenic barriers: 10-50% of the margin of a plant species' range is
bordered by intensive urban development; 25% of the streams occupied by a fish
species include dams that are likely to impede range shifts driven by climate
change.  </p>

<p> Neutral: Significant
barriers do not exist for this species, OR small barriers exist in the
assessment area but likely would not significantly impair distributional shifts
with climate change, OR substantial barriers exist but are not likely to
contribute significantly to a reduction or loss of the species' habitat or area
of occupancy with projected climate change in the assessment area.  </p>

<p> Examples
of species in this category: most birds (for which barriers do not exist);
terrestrial snakes in extensive plains or deserts that may have small barriers
that would not impede distributional shifts with climate change; small
alpine-subalpine mammal (e.g., ermine, snowshoe hare) in extensive mountainous
wilderness area lacking major rivers or lakes; fishes in large deep lakes or
large main-stem rivers that are basically invulnerable to projected climate
change and lack dams, waterfalls, and significant pollution; a plant whose
climate envelope is shifting northward and range is bordered on the west by a
barrier but for which no barriers exist to the north.  </p>

<p>  </p>

<p> 3) Predicted Impact of Land Use Changes Resulting from
Human Responses to Climate Change   </p>

<p> (e.g., plantations for carbon
offsets, new seawalls in response to sea level rise, and renewable energy
projects such as wind-farms, solar arrays, or biofuels production)   </p>

<p> NOTES: Strategies designed to mitigate or
adapt to climate change have the potential to affect very large areas of land,
and the species that depend on these areas, in both positive and negative ways.
This factor arguably should be considered in conservation status assessments,
but considering that for most species this factor has not yet been considered
in these assessments, we include it here. If the land use changes for
alternative energy projects have already been considered in the conservation
status assessment for the species, consider not scoring this factor, especially
if the vulnerability assessment results will be used to revise status ranks.  </p>

<p> This factor is NOT intended to
include habitat loss or destruction due to on-going human activities, as these
should already be reflected in existing conservation status ranks. Include only
new activities related directly to climate change mitigation here. There is
much uncertainty about the types of mitigation action that are likely to
threaten habitats and species. Remember that multiple categories can be checked
for each factor to capture uncertainty. As federal and state climate change
legislation is enacted, some of the mitigation directions (and associated
threats or benefits to species) will become clearer.    </p>

<p> TOOLS: For a map of clean energy sites in
the western U.S. see http://www.nrdc.org/land/sitingrenewables/usersguide.asp.
A library of energy-related maps can be found at
http://www.nrel.gov/gis/mapsearch/.  </p>

<p> Increase
Vulnerability: The natural history/requirements of the species are known to
be incompatible with mitigation-related land use changes that are likely to
very likely to occur within its current and/or potential future range. This
includes (but is not limited to) the following:  </p>

<p>
- Species requiring open habitats within landscapes likely to be reforested or
afforested. If the species requires openings within forests that are
created/maintained by natural processes (e.g., fire), and if those processes
have a reasonable likelihood of continuing to operate within its range, a
lesser impact category may be appropriate.  </p>

<p>
- Bird and bat species whose migratory routes, foraging territory, or lekking
sites include existing and/or suitable wind farm sites and for which studies
indicate substantial negative impact (e.g., mortality from or avoidance of
turbines). If such studies indicate a relatively low impact from wind energy
development, a lesser impact category may be appropriate.  </p>

<p>
- Greater than 20% of the species' range within the assessment area occurs on
marginal agricultural land, such as CRP land or other open areas with suitable
soils for agriculture (&quot;prime farmland&quot;, etc.) that are not currently
in agricultural production OR &gt; 50% of the species' range within the
assessment area occurs on any non-urbanized land with suitable soils, where
there is a reasonable expectation that such land may be converted to biofuel
production.  </p>

<p>
- The species occurs in one or more river/stream reaches not yet developed for
hydropower, but with the potential to be so developed.  </p>

<p>
- Species of deserts or other permanently open, flat lands with potential for
placement of solar arrays.   </p>

<p>
- Species dependent on dynamic shoreline habitats (e.g., active dunes or salt
marshes) likely to be destroyed by human fortifications against rising sea
levels.  </p>

<p> Somewhat
Increase Vulnerability: The natural history/requirements of the
species are known to be incompatible with mitigation-related land use changes
that may possibly occur within its current and/or potential future range,
including any of the above (under Increase).  </p>

<p> Neutral: The
species is unlikely to be significantly affected by mitigation-related land use
changes that may occur within its current and/or potential future range,
including any of the above; OR it is unlikely that any mitigation-related land
use changes will occur within the species' current and/or potential future
range; OR it may benefit from mitigation-related land use changes.</div>")
        #tags$iframe("www/guide.pdf")
      ))
    })

    observeEvent(input$guideC, {
      showModal(modalDialog(
        title = "Section C Guidelines",
        #includeHTML("C:/Users/endicotts/Documents/Definitions and Guidelines for Scoring Risk Factors.html")
      ))
    })

    observeEvent(input$guideD, {
      showModal(modalDialog(
        title = "Section D Guidelines",
        #includeHTML("C:/Users/endicotts/Documents/Definitions and Guidelines for Scoring Risk Factors.html")
      ))
    })

    # When next button is clicked move to next panel
    observeEvent(input$nextVuln, {
      updateTabsetPanel(session, "tabset",
                        selected = "Spatial Vulnerability Questions")
      shinyjs::runjs("window.scrollTo(0, 0)")
    })

    # Spatial Vulnerability Questions #========================
    # C2ai
    observe({
      req(input$nextVuln)
      if(isTruthy(clim_vars()$htn)){
        shinyjs::hide("missing_htn")
        shinyjs::show("map_C2ai")
      } else {
        shinyjs::hide("map_C2ai")
        shinyjs::show("missing_htn")
      }
    })

    output$map_C2ai <- tmap::renderTmap({
      req(input$nextVuln)
      req(clim_vars()$htn)

      make_map(range_poly(), rast = clim_vars()$htn, rast_nm = "htn")
    })

    output$tbl_C2ai <- renderTable({
      exp_df <-  spat_res() %>%
        select(contains("HTN")) %>%
        rename_at(vars(contains("HTN")),
                  ~stringr::str_replace(.x, "HTN_", "Class ")) %>%
        tidyr::pivot_longer(cols = contains("Class"),
                     names_to = "Change Class", values_to = "Proportion of Range") %>%
        transmute(`Change Class`, `Proportion of Range`)
    })

    output$box_C2ai <- renderUI({
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

    # C2aii
    observe({
      req(input$nextVuln)
      if(isTruthy(clim_vars()$ptn)){
        shinyjs::hide("missing_ptn")
        shinyjs::show("map_C2aii")
      } else {
        shinyjs::hide("map_C2aii")
        shinyjs::show("missing_ptn")
      }
    })

    output$map_C2aii <- tmap::renderTmap({
      req(input$nextVuln)
      req(clim_vars()$ptn)

      make_map(poly1 = range_poly(), poly2 = clim_vars()$ptn, poly2_nm = "ptn")
    })

    output$tbl_C2aii <- renderTable({
      exp_df <-  spat_res() %>%
        select(contains("PTN")) %>%
        tidyr::pivot_longer(cols = contains("PTN"),
                     names_to = "Variable", values_to = "Proportion of Range")
    })

    output$box_C2aii <- renderUI({
      box_val <- spat_res() %>%
        mutate(C2aii = case_when(PTN > 90 ~ 3,
                                 PTN > 50 ~ 2,
                                 PTN > 10 ~ 1,
                                 is.na(PTN) ~ NA_real_,
                                 TRUE ~ 0)) %>%
        pull(C2aii)

      checkboxGroupInput("C2aii", HTML("Calculated effect on vulnerability. <font color=\"#FF0000\"><b> Editing this response will override the results of the spatial analysis.</b></font>"),
                         choiceNames = valueNms,
                         choiceValues = valueOpts,
                         selected = box_val,
                         inline = TRUE)
    })

    # C2bi
    observe({
      req(input$nextVuln)
      if(isTruthy(clim_vars()$map)){
        shinyjs::hide("missing_map")
        shinyjs::show("map_C2bi")
      } else {
        shinyjs::hide("map_C2bi")
        shinyjs::show("missing_map")
      }
    })

    output$map_C2bi <- tmap::renderTmap({
      req(input$nextVuln)
      req(clim_vars()$map)

      make_map(poly1 = range_poly(), rast = clim_vars()$map, rast_nm = "map",
               rast_style = "pretty")
    })

    output$tbl_C2bi <- renderTable({
      exp_df <-  spat_res() %>%
        select(MAP_max, MAP_min) %>%
        rename(`Min MAP` = MAP_min, `Max MAP` = MAP_max)
    })

    output$box_C2bi <- renderUI({
      box_val <- spat_res() %>%
        mutate(range_MAP = MAP_max - MAP_min,
               C2bi = case_when(range_MAP < 100 ~ 3,
                                range_MAP < 254 ~ 2,
                                range_MAP < 508 ~ 1,
                                is.na(range_MAP) ~ NA_real_,
                                TRUE ~ 0)) %>%
        pull(C2bi)

      checkboxGroupInput("C2bi", HTML("Calculated effect on vulnerability. <font color=\"#FF0000\"><b> Editing this response will override the results of the spatial analysis.</b></font>"),
                         choiceNames = valueNms,
                         choiceValues = valueOpts,
                         selected = box_val,
                         inline = TRUE)
    })

    # D2 and D3
    observe({
      req(input$nextVuln)
      if(isTruthy(hs_rast())){
        shinyjs::hide("missing_hs")
        shinyjs::show("map_D2_3")
      } else {
        shinyjs::hide("map_D2_3")
        shinyjs::show("missing_hs")
      }
    })

    output$map_D2_3 <- tmap::renderTmap({
      req(input$nextVuln)
      req(hs_rast())

      make_map(poly1 = range_poly(), rast = hs_rast(), rast_nm = "hs_rast")
    })

    output$tbl_D2_3 <- renderTable({
      exp_df <-  spat_res() %>%
        select(`% Lost` = perc_lost, `% Gain` = perc_gain,
               `% Maintained` = perc_maint)
    })

    output$box_D2 <- renderUI({
      box_val <- spat_res() %>%
        mutate(D2 = case_when(perc_lost > 99 ~ 3,
                              perc_lost > 50 ~ 2,
                              perc_lost > 20 ~ 1,
                              is.na(perc_lost) ~ NA_real_,
                              TRUE ~ 0)) %>%
        pull(D2)

      checkboxGroupInput("D2", HTML("Calculated effect on vulnerability. <font color=\"#FF0000\"><b> Editing this response will override the results of the spatial analysis.</b></font>"),
                         choiceNames = valueNms,
                         choiceValues = valueOpts,
                         selected = box_val,
                         inline = TRUE)
    })

    output$box_D3 <- renderUI({
      box_val <- spat_res() %>%
        mutate(D2 = case_when(perc_lost > 99 ~ 3,
                              perc_lost > 50 ~ 2,
                              perc_lost > 20 ~ 1,
                              is.na(perc_lost) ~ NA_real_,
                              TRUE ~ 0),
               D3 = case_when(D2 == 3 ~ 0,
                              perc_maint == 0 ~ 3,
                              perc_maint < 30 ~ 2,
                              perc_maint < 60 ~ 1,
                              is.na(perc_maint) ~ NA_real_,
                              TRUE ~ 0)) %>%
        pull(D3)

      checkboxGroupInput("D3", HTML("Calculated effect on vulnerability. <font color=\"#FF0000\"><b> Editing this response will override the results of the spatial analysis.</b></font>"),
                         choiceNames = valueNms,
                         choiceValues = valueOpts,
                         selected = box_val,
                         inline = TRUE)
    })

    # When submit button is clicked move to next panel
    observeEvent(input$submitSpatVuln, {
      updateTabsetPanel(session, "tabset",
                        selected = "Results"
      )
      shinyjs::runjs("window.scrollTo(0, 0)")
    })

    # Calculate Index value #================================

    # Gather all the form inputs
    vuln_df <- eventReactive(input$submitSpatVuln, {
      vuln_qs <- stringr::str_subset(names(input), "^[B,C,D]\\d.*")
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
                       ind == "MV" ~ "#FFC125",
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
    output$q_score_plt <- plotly::renderPlotly({
      plot_q_score(index_res()$vuln_df)
    })

    out_data <- reactive({
      vuln_df <- index_res()$vuln_df %>%
        select(Code, contains("Value")) %>%
        filter(!Code %in% c("Z2", "Z3")) %>%
        arrange(Code) %>%
        mutate_all(as.character) %>%
        tidyr::unite(Value, Value1:Value4, na.rm = TRUE, sep = ", ") %>%
        tidyr::pivot_wider(names_from = "Code", values_from = "Value")

      spat_df <- spat_res()

      conf_df <- index_res()$index_conf %>%
        mutate(index = paste0("MC_freq_", index)) %>%
        tidyr::pivot_wider(names_from = "index", values_from = "frequency")

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
}

