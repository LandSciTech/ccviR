# based on example app: https://github.com/daattali/shiny-server/blob/master/mimic-google-form/app.R
# and blog post explaining it: https://deanattali.com/2015/06/14/mimicking-google-form-shiny/

#' Create the ccviR Shiny application
#'
#'
#'
#' @noRd
ccvi_app <- function(testmode_in, ...){
  # which fields are mandatory
  fieldsMandatory1 <- c("assessor_name", "geo_location", "tax_grp", "species_name")

  fieldsMandatory2 <- c("range_poly_pth", "assess_poly_pth")

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

  # set theme
  my_theme <- ggplot2::theme_classic() +
    ggplot2::theme(text = ggplot2::element_text(size = 12),
          strip.background = ggplot2::element_blank())

  ggplot2::theme_set(my_theme)

  # Let tmap try to fix polygons that are invalid
  tmap::tmap_options(check.and.fix = TRUE)

  # Header #=================================
  ui <-  function(request){
    fluidPage(
      shinyjs::useShinyjs(),
      shinyjs::inlineCSS(appCSS),
      title = "ccviR app",
      tags$head(tags$style(type = "text/css",
                           ".container-fluid {  max-width: 1050px; /* or 1050px */}")),
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

      navlistPanel(
        id = "tabset",
        well = FALSE,
        widths = c(3, 9),
        # Introduction #===============
        tabPanel(
          "Welcome",
          fluidPage(
            h2("Welcome"),
            p("This app provides a new interface for the Climate Change Vulnerability Index (CCVI) created by ",
              a("NatureServe", href = "https://www.natureserve.org/conservation-tools/climate-change-vulnerability-index"),
              "that automates the spatial analysis needed to inform the index. ",
              "The app is based on version 3.02 of the NatureServe CCVI. ",
              "See the app ",
              a("tutorial", href = "https://landscitech.github.io/ccviR/articles/app_vignette.html"),
              "for a demonstration of how to use the app. ",
              "For detailed instructions on how to use the index and definitions ",
              "of the terms used below see the ",
              a("NatureServe Guidelines.", href = "https://www.natureserve.org/sites/default/files/guidelines_natureserveclimatechangevulnerabilityindex_r3.02_1_jun_2016.pdf"),
              "Required datasets are indicated with ", labelMandatory("a"), "."),
            h3("Preparing to use the app"),

            p(strong("Step 0: "),"The first time you use the app ",
              "you will need to download the climate data used in the app or",
              " prepare your own using raw climate data and the ",
              a("data preparation app.", href = "https://landscitech.github.io/ccviR/articles/data_prep_vignette.html")),
            p(strong("Step 1: "), "Acquire species-specific spatial datasets:",
              tags$ul(
                tags$li(labelMandatory("Species North American or global range polygon")),
                tags$li(labelMandatory("Assessment area polygon")),
                tags$li("Non-breeding range polygon"),
                tags$li("Projected range change raster"),
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
            h3("Start assessment"),
            actionButton("start", "Start", class = "btn-primary"),
            br(),
            br(),
            strong("Or load data from a previous assessment"),
            br(),
            load_bookmark_ui("load"),
            br(),
            br(),
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
                labelMandatory(strong("Folder location of prepared climate data:")),
                shinyDirButton("clim_var_dir", "Choose a folder",
                               "Folder location of prepared climate data"),
                shinycssloaders::withSpinner(verbatimTextOutput("clim_var_dir_out", placeholder = TRUE), proxy.height = "100px"),
                verbatimTextOutput("clim_var_error"),
                br(),
                get_file_ui("range_poly_pth", "Range polygon shapefile", mandatory = TRUE),
                get_file_ui("assess_poly_pth", "Assessment area polygon shapefile", mandatory = TRUE),
                get_file_ui("ptn_poly_pth", "Physiological thermal niche file"),
                get_file_ui("nonbreed_poly_pth", "Non-breeding Range polygon shapefile"),
                selectInput("rng_chg_used", "Will a projected range change raster be supplied?",
                            c("No" = "no",
                              "Yes, one range change raster will be supplied for all scenarios" = "one",
                              "Yes, multiple range change rasters will be supplied, one for each scenario (Preferred)" = "multiple")),
                uiOutput("rng_chg_sel_ui"),
                conditionalPanel(condition = "input.rng_chg_used !== 'no'",
                                 strong("Classification of projected range change raster"),
                                 p("Enter the range of values in the raster corresponding to ",
                                   "lost, maintained, gained and not suitable."),
                                 strong("Lost: "),
                                 tags$div(numericInput("lost_from", "From", 1), style="display:inline-block"),
                                 tags$div(numericInput("lost_to", "To", 1), style="display:inline-block"),
                                 br(),
                                 strong("Maintained: "),
                                 tags$div(numericInput("maint_from", "From", 2), style="display:inline-block"),
                                 tags$div(numericInput("maint_to", "To", 6), style="display:inline-block"),
                                 br(),
                                 strong("Gained: "),
                                 tags$div(numericInput("gain_from", "From", 7), style="display:inline-block"),
                                 tags$div(numericInput("gain_to", "To", 7), style="display:inline-block"),
                                 br(),
                                 strong("Not Suitable: "),
                                 tags$div(numericInput("ns_from", "From", 0), style="display:inline-block"),
                                 tags$div(numericInput("ns_to", "To", 0), style="display:inline-block"),
                                 br(), br(),
                                 strong("Gain modifier"),
                                 p("Range gains predicted based on future climate projections should be ",
                                   "interpreted cautiously. It is important to consider whether ",
                                   "the gains are likely to be realized. E.g. Is the species ",
                                   "capable of dispersing to the new habitat and will factors other ",
                                   "than climate be suitable in those areas?"),
                                 p("To account for this you can set the gain modifier below to less",
                                   " than 1 in order to down weight future range expansions when the",
                                   " modelled response to climate change is being assessed. A value of 0 will ",
                                   "ignore gains all together while 0.5 assumes that only half",
                                   " the projected gains are realized and 1 assumes all gains are realized."),
                                 numericInput("gain_mod", NULL, 1, min = 0, max = 1, step = 0.1),
                                 textAreaInput("gain_mod_comm", "Gain modifier explanation")
                                 ),
                strong("Click Run to begin the spatial analysis or to re-run it",
                       " after changing inputs"),
                br(),
                actionButton("startSpatial", "Run", class = "btn-primary"),
                br(),
                conditionalPanel(
                  condition = "input.startSpatial > 0",
                  shinycssloaders::withSpinner(verbatimTextOutput("spat_error"),
                                               proxy.height = "50px"),
                  actionButton("next2", "Next", class = "btn-primary"),
                  br(), br()
                )
              )
            )
          )
        ),
        # Exposure Results #====================================================
        tabPanel(
          "Exposure Results",
          fluidRow(
            column(
              12,
              div(
                id = "texp_map_div",
                h3("Temperature exposure"),
                shinycssloaders::withSpinner(tmap::tmapOutput("texp_map")),
                tableOutput("texp_tbl")
              ),
              div(
                id = "cmd_map",
                h3("Moisture exposure"),
                tmap::tmapOutput("cmd_map"),
                tableOutput("cmd_tbl")
              ),
              div(
                h3("Migratory exposure - Climate change exposure index"),
                div(id = "missing_ccei",
                    HTML("<font color=\"#FF0000\"><b>Data set not provided.</b></font> <br>CCEI data and a non-breeding range are needed to calculate."),
                    br(),
                    br()),
                div(
                  id = "ccei_exp",
                  tmap::tmapOutput("ccei_map"),
                  tableOutput("tbl_ccei"))

              )
            )
          ),
          fluidRow(
            column(
              12,
              actionButton("next3", "Next", class = "btn-primary"),
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
                check_comment_ui("B1", "1) Exposure to sea level rise:",
                                 choiceNames = valueNms,
                                 choiceValues = valueOpts),
                check_comment_ui("B2a", "2a) Distribution relative to natural barriers",
                                 choiceNames = valueNms,
                                 choiceValues = valueOpts),
                check_comment_ui("B2b", "2b) Distribution relative to anthropogenic barriers",
                                 choiceNames = valueNms,
                                 choiceValues = valueOpts),

                check_comment_ui("B3", "  3) Predicted impact of land use changes resulting from human responses to climate change",
                                 choiceNames = valueNms[2:4],
                                 choiceValues = valueOpts[2:4])
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
                check_comment_ui("C1", "1) Dispersal and movements",
                                 choiceNames = valueNms,
                                 choiceValues = valueOpts),

                strong("2b) Predicted sensitivity to changes in precipitation, hydrology, or moisture regime:"),

                check_comment_ui("C2bii", "ii) physiological hydrological niche.",
                                 choiceNames = valueNms,
                                 choiceValues = valueOpts),

                check_comment_ui("C2c", "2c) Dependence on a specific disturbance regime likely to be impacted by climate change.",
                                 choiceNames = valueNms[2:4],
                                 choiceValues = valueOpts[2:4]),
                check_comment_ui("C2d", "2d) Dependence on ice, ice-edge, or snow-cover habitats.",
                                 choiceNames = valueNms,
                                 choiceValues = valueOpts),

                check_comment_ui("C3", "3) Restriction to uncommon landscape/geological features or derivatives.",
                                 choiceNames = valueNms[2:4],
                                 choiceValues = valueOpts[2:4]),
                check_comment_ui("C4a", "4a) Dependence on other species to generate required habitat.",
                                 choiceNames = valueNms[2:4],
                                 choiceValues = valueOpts[2:4]),
                shinyjs::hidden(
                  div(
                    id = "animal_only",
                    check_comment_ui("C4b", "4b) Dietary versatility (animals only).",
                                     choiceNames = valueNms[2:4],
                                     choiceValues = valueOpts[2:4])
                  )
                ),
                shinyjs::hidden(
                  div(
                    id = "plant_only",
                    check_comment_ui("C4c", "4c) Pollinator versatility (plants only).",
                                     choiceNames = valueNms[2:4],
                                     choiceValues = valueOpts[2:4])
                  )
                ),
                check_comment_ui("C4d", "4d) Dependence on other species for propagule dispersal.",
                                 choiceNames = valueNms[2:4],
                                 choiceValues = valueOpts[2:4]),
                check_comment_ui("C4e", "4e) Sensitivity to pathogens or natural enemies.",
                                 choiceNames = valueNms[2:4],
                                 choiceValues = valueOpts[2:4]),
                check_comment_ui("C4f", "4f) Sensitivity to competition from native or non-native species.",
                                 choiceNames = valueNms[2:4],
                                 choiceValues = valueOpts[2:4]),
                check_comment_ui("C4g", "4g) Forms part of an interspecific interaction not covered by 4a-f.",
                                 choiceNames = valueNms[2:4],
                                 choiceValues = valueOpts[2:4]),

                check_comment_ui("C5a", "5a) Measured genetic variation.",
                                 choiceNames = valueNms[2:4],
                                 choiceValues = valueOpts[2:4]),
                conditionalPanel(
                  "input.C5a == ''",
                  check_comment_ui("C5b", "5b) Occurrence of bottlenecks in recent evolutionary history (use only if 5a is unknown).",
                                   choiceNames = valueNms[2:4],
                                   choiceValues = valueOpts[2:4]),

                ),
                conditionalPanel(
                  "input.C5a == '' && input.C5b == ''",
                  shinyjs::hidden(
                    div(
                      id = "plant_only2",
                      check_comment_ui("C5c", "5c) Reproductive system (plants only; use only if C5a and C5b are unknown).",
                                       choiceNames = valueNms[2:4],
                                       choiceValues = valueOpts[2:4])
                    )
                  )
                ),

                check_comment_ui("C6", "6) Phenological response to changing seasonal temperature and precipitation dynamics.",
                                 choiceNames = valueNms[2:4],
                                 choiceValues = valueOpts[2:4])
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

                check_comment_ui("D1", "1) Documented response to recent climate change. ",
                                 choiceNames = valueNms,
                                 choiceValues = valueOpts),

                check_comment_ui("D4", "4) Occurrence of protected areas in modeled future distribution.",
                                 choiceNames = valueNms[2:4],
                                 choiceValues = valueOpts[2:4]),
                actionButton("next4", "Next", class = "btn-primary"),
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
              actionButton("guideC2", "Show guidelines"),
              br(),
              spat_vuln_ui(
                id = "C2ai",
                header = "Predicted sensitivity to temperature and moisture changes:",
                vuln_q_nm = "2a) i) Historical thermal niche."
              ),
              spat_vuln_ui(
                id = "C2aii",
                vuln_q_nm = "2a) ii) Physiological thermal niche."
              ),
              spat_vuln_ui(
                id = "C2bi",
                vuln_q_nm = "2b) i) Historical hydrological niche."
              ),
              h4("Section D: Documented or Modeled Response to Climate Change"),
              actionButton("guideD2", "Show guidelines"),
              br(),
              spat_vuln_ui(
                id = "D2_3",
                header = "Modeled future range change",
              ),
              strong("2) Modeled future (2050) change in population or range size."),
              uiOutput("box_D2"),
              strong("3) Overlap of modeled future (2050) range with current range"),
              uiOutput("box_D3"),
              actionButton("next5", "Next", class = "btn-primary")
            )
          )
        ),
        # Results #===================================
        tabPanel(
          "Index Results",
          fluidPage(
            div(
              id = "formData",
              #style = 'width:800px;',
              h3("Results"),
              h5("Click the button to calculate or re-calculate the index"),
              actionButton("calcIndex", "Calculate", class = "btn-primary")
              ),

              conditionalPanel(
                condition = "output.calcFlag == true",
                h4("Data completeness"),
                tableOutput("n_factors"),

                h4("Variation in index"),
                p("When multiple values are selected for any of the vulnerability ",
                  "factors the average of the values is used to calculate the ",
                  "overall index. To test the uncertainty in the result a Monte Carlo ",
                  "simulation with 1000 runs is carried out. In each simulation run ",
                  "one of the selected values is randomly chosen and the index is ",
                  "calculated. The graph below shows the proportion of runs with each",
                  " index value for each scenario. "),
                # p("Confidence in the index is:",
                #   textOutput("conf_index", inline = TRUE)),
                plotOutput("conf_graph", width = 300, height = 200),
              div(
                id = "indplt",
                #style = 'width:800px;',
                br(),
                h4("Factors contributing to index value"),
                p("The CCVI is calculated by combining the index calculated based on ",
                  "exposure, sensitivity and adaptive capacity with the index ",
                  "calculated based on documented or modelled responses to climate change. ",
                  "The plot below demonstrates which of these had the strongest",
                  "influence on the overall calculated index. The lines indicate",
                  " the range of scores produced by the Monte Carlo simulations. ",
                  "A score of negative one on the vertical ",
                  "axis indicates none of the factors in the modelled response to",
                  " climate change section were completed"),
                # Might want to add something like this to change width dependent
                # on n facets https://stackoverflow.com/questions/50914398/increase-plot-size-in-shiny-when-using-ggplot-facets

                plotOutput("ind_score_plt", height = "300px"),
                textOutput("slr"),
                br(), br(),
                p("The score for each vulnerability factor is determined by the ",
                  "answers to vulnerability questions (Neutral: 0, Greatly increases: 3)",
                  "multiplied by the exposure multiplier for temperature or moisture,",
                  "whichever is most relevant to that factor. When multiple values ",
                  "are selected for any of the vulnerability ",
                  "factors the average of the values is used. These scores are summed ",
                  "to determine the index. The plot below demonstrates which factors ",
                  "had the highest scores and how exposure impacted the score."),
                plotly::plotlyOutput("q_score_plt")
              ),

              # helpful for testing
              # shinyjs::runcodeUI(),

              br(), br(),
              downloadButton("downloadData", "Download results as csv"),
              downloadButton("downloadDefs", "Download column definitions"),
              br(), br(),
              actionButton("restart", "Assess another species",
                           class = "btn-primary"),
              br(),
              br(),
              save_bookmark_ui("save")
            )
          )
        )
      ),
      div(
        id = "footer",
        br(),
        br())
    )
  }

  # Server #========================
  server <- function(input, output, session) {
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

    # Flag for if this is a restored session
    restored <- reactiveValues()

    observeEvent(input$start, {
      updateTabsetPanel(session, "tabset",
                        selected = "Species Information"
      )
      shinyjs::runjs("window.scrollTo(0, 0)")
    })

    # restore a previous session
    load_bookmark_server("load", volumes)

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
                 isTruthy(file_pths()[[x]]) & isTruthy(clim_dir_pth())
               },
               logical(1))
      mandatoryFilled2 <- all(mandatoryFilled2)
      if (isTRUE(getOption("shiny.testmode"))) {
       mandatoryFilled2 <- TRUE
      }

      shinyjs::toggleState(id = "startSpatial", condition = mandatoryFilled2)
      shinyjs::toggleState(id = "next2", condition = mandatoryFilled2 & isTruthy(spat_res()))
    })

    # update filePathIds based on selection for rng_chg
    filePathIds <- reactive({
      # File path ids to use with file choose
      fileIds <- c("range_poly_pth", "nonbreed_poly_pth", "assess_poly_pth", "ptn_poly_pth")
      names(fileIds) <- fileIds

      rng_chg_pths <- stringr::str_subset(names(input), "rng_chg_pth")

      if(length(rng_chg_pths) > 0){
        names(rng_chg_pths) <- rng_chg_pths

        return(c(fileIds, rng_chg_pths))
      } else {
        return(fileIds)
      }

    })

    # Find file paths
    shinyDirChoose(input, "clim_var_dir", root = volumes)
    observe({
      purrr::map(filePathIds(), shinyFileChoose, root = volumes, input = input,
                 filetypes = c("shp", "tif", "asc", "nc", "grd", "bil"))
    })

    # parse file paths
    clim_dir_pth <- reactive({
      if(is.integer(input$clim_var_dir)){
        if(!is.null(restored$yes)){
          return(clim_dir_pth_restore())
        } else if (isTRUE(getOption("shiny.testmode"))) {
          return(system.file("extdata/clim_files/processed", package = "ccviR"))
        } else {
          return(NULL)
        }
      }  else {
        return(parseDirPath(volumes, input$clim_var_dir))
      }
    })


    file_pths <- reactive({
      purrr::map(filePathIds(), ~{
        if(is.integer(input[[.x]])){
          if(!is.null(restored$yes)){
            return(file_pths_restore()[[.x]])
          }
            return(NULL)
          } else {
          return(parseFilePaths(volumes, input[[.x]])$datapath)
        }

      })
    })

    # output file paths
    output$clim_var_dir_out <- renderText({
      clim_dir_pth()
    })

    observe({
      purrr::walk2(file_pths(), filePathIds(), ~{
        out_name <- paste0(.y, "_out")
        output[[out_name]] <- renderText({.x})
      })
    })

    # load spatial data
    clim_readme <- reactive({
      req(clim_dir_pth())
      if(!file.exists(fs::path(clim_dir_pth(), "climate_data_readme.csv"))){
        stop("The climate folder is missing the required readme file",
             call. = FALSE)
      }
      utils::read.csv(fs::path(clim_dir_pth(), "climate_data_readme.csv"),
                      check.names = FALSE)
    })

    clim_vars <- reactive({
      root_pth <- clim_dir_pth()

      req(root_pth)
      req(clim_readme)
      print(clim_readme()$Scenario_Name)

      clim_vars <- try(get_clim_vars(root_pth, scenario_names = clim_readme()$Scenario_Name))

    })

    range_poly_in <- reactive({
      if (isTRUE(getOption("shiny.testmode"))) {
        sf::st_read(system.file("extdata/rng_poly_high.shp",
                                package = "ccviR"),
                    agr = "constant", quiet = TRUE)
      } else {
        sf::st_read(file_pths()$range_poly_pth,
                    agr = "constant", quiet = TRUE)
      }

    })

    nonbreed_poly <- reactive({
      if (isTRUE(getOption("shiny.testmode"))) {
        pth <- system.file("extdata/nonbreed_poly.shp",
                           package = "ccviR")
      } else {
        pth <- file_pths()$nonbreed_poly_pth
      }

      if(!isTruthy(pth)){
        return(NULL)
      }
      sf::st_read(pth, agr = "constant", quiet = TRUE)
    })

    assess_poly <- reactive({
      if (isTRUE(getOption("shiny.testmode"))) {
        sf::st_read(system.file("extdata/assess_poly.shp",
                                package = "ccviR"),
                    agr = "constant", quiet = TRUE)
      } else {
        sf::st_read(file_pths()$assess_poly_pth,
                    agr = "constant", quiet = TRUE)
      }
    })

    # use readme to render scenario names for rng chg rasters
    output$rng_chg_sel_ui <- renderUI({
      if(input$rng_chg_used == "no"){
        return(NULL)
      } else if(input$rng_chg_used == "one"){
        get_file_ui("rng_chg_pth", "Projected range change raster")
      } else if (input$rng_chg_used == "multiple"){
        tagList(
          strong("Select a projected range change raster for each scenario"),
          purrr::map2(clim_readme()$Scenario_Name,
                      1:length(clim_readme()$Scenario_Name),
                      ~get_file_ui(paste0("rng_chg_pth", "_", .y), .x)),
          br(), br()
          )

      }
    })

    hs_rast <- reactive({
      if (isTRUE(getOption("shiny.testmode"))) {
        pth <- system.file("extdata/HS_rast_high.tif",
                           package = "ccviR")
      } else {
        pth <- file_pths()[stringr::str_subset(names(input), "rng_chg_pth")] %>%
          unlist()
        pth <- pth[sort(names(pth))]
      }

      if(!isTruthy(pth) || length(pth) == 0){
        return(NULL)
      }else {
        names(pth) <- fs::path_file(pth) %>% fs::path_ext_remove()

        check_trim(raster::stack(pth))
      }


    })

    ptn_poly <- reactive({
      if (isTRUE(getOption("shiny.testmode"))) {
        pth <- system.file("extdata/ptn_poly.shp", package = "ccviR")
      } else {
        pth <- file_pths()$ptn_poly_pth
      }
      if(!isTruthy(pth)){
        return(NULL)
      }
      sf::st_read(pth, agr = "constant", quiet = TRUE)
    })

    # assemble hs_rcl matrix
    hs_rcl_mat <- reactive({
      mat <- matrix(c(input$lost_from, input$lost_to, 1,
                                     input$maint_from, input$maint_to, 2,
                                     input$gain_from, input$gain_to, 3,
                                     input$ns_from, input$ns_to, 0),
                                   byrow = TRUE, ncol = 3)

      # if an input is blank then the value is NA but that converts raster values that
      # are NA to that value
      mat[which(!is.na(mat[, 1])), ]
    })

    doSpatial <- reactiveVal(FALSE)

    observe({
      if(!is.null(restored$yes)){
        doSpatial(1)
        message("doSpatial restore")
      }
    })

    observeEvent(input$startSpatial, {
      showModal(modalDialog(
        p("Note: Re-running the spatial analysis will overwrite any changes made to ",
          "the Spatial Vulnerability Questions. Comments will be preserved so ",
          "you can record the change made in the comments and then change it ",
          "again after re-running the analysis."),
        footer = tagList(
          actionButton("shinyalert", "Continue"),
          modalButton("Cancel")
        ),
        title = "Do you want to run the spatial analysis?"))
      if(input$startSpatial == 1){
        shinyjs::click("shinyalert")
      }
    })

    observeEvent(input$shinyalert, {
      removeModal()
      if(input$shinyalert > 0){
        doSpatial(doSpatial() +1)
      }
      shinyjs::runjs("window.scrollTo(0, document.body.scrollHeight)")
    })

    # run spatial calculations
    spat_res1 <- reactive({
      req(doSpatial())
      req(clim_vars())
      isolate({
        tryCatch({
          run_spatial(range_poly = range_poly_in(),
                      non_breed_poly = nonbreed_poly(),
                      scale_poly = assess_poly(),
                      hs_rast = hs_rast(),
                      ptn_poly = ptn_poly(),
                      clim_vars_lst = clim_vars(),
                      hs_rcl = hs_rcl_mat(),
                      gain_mod = input$gain_mod,
                      scenario_names = clim_readme()$Scenario_Name)
        },
        error = function(cnd) conditionMessage(cnd))
      })

    })

    range_poly <- reactive({
      req(doSpatial())
      req(!is.character(spat_res1()))
      spat_res1()$range_poly_assess
    })
    range_poly_clim <- reactive({
      req(doSpatial())
      req(!is.character(spat_res1()))
      spat_res1()$range_poly_clim
    })
    spat_res <- reactive({
      req(doSpatial())
      req(!is.character(spat_res1()))
      spat_res1()$spat_table
    })

    output$clim_var_error <- renderText({
      if(inherits(clim_vars(), "try-error")){
        stop(conditionMessage(attr(clim_vars(), "condition")))
      }
    })

    output$spat_error <- renderText({
      if(inherits(hs_rast(), "try-error")){
        stop(conditionMessage(attr(hs_rast(), "condition")))
      }
      if(is.character(spat_res1())){
        stop(spat_res1(), call. = FALSE)
      } else {
        "Spatial analysis complete"
      }
    })

    # Make maps
    output$texp_map <- tmap::renderTmap({
      req(!is.character(spat_res()))

      isolate(
        make_map(range_poly(), clim_vars()$mat, rast_nm = "mat",
                 rast_lbl = c("1 High", "2", "3","4", "5", "6 Low"))
      )
    })

    observe({
      req(doSpatial())
      if(isTruthy(clim_vars()$ccei) && isTruthy(isolate(nonbreed_poly()))){
        shinyjs::hide("missing_ccei")
        shinyjs::show("ccei_exp")
      } else {
        shinyjs::hide("ccei_exp")
        shinyjs::show("missing_ccei")
      }
    })

    output$ccei_map <- tmap::renderTmap({
      req(!is.character(spat_res()))
      req(clim_vars()$ccei)
      req(isolate(nonbreed_poly()))
      isolate(
        make_map(nonbreed_poly(), clim_vars()$ccei, rast_nm = "ccei",
                 rast_lbl = c("1 Low", "2", "3", "4 High"))
      )
    })

    output$cmd_map <- tmap::renderTmap({
      req(!is.character(spat_res()))
      isolate(
        make_map(range_poly(), clim_vars()$cmd, rast_nm = "cmd",
                 rast_lbl = c("1 High", "2", "3","4", "5", "6 Low"))
      )
    })

    output$texp_tbl <- renderTable({
      req(!is.character(spat_res()))
      exp_df <-  spat_res() %>% rowwise() %>%
        mutate(temp_exp = case_when(
          MAT_6 > 50 ~ 2.4,
          sum(MAT_6, MAT_5, na.rm = TRUE) >= 75 ~ 2,
          sum(MAT_6, MAT_5, MAT_4, na.rm = TRUE) >= 60 ~ 1.6,
          sum(MAT_6, MAT_5, MAT_4, MAT_3, na.rm = TRUE) >= 40 ~ 1.2,
          sum(MAT_6, MAT_5, MAT_4, MAT_3, MAT_2, na.rm = TRUE) >= 20 ~ 0.8,
          TRUE ~ 0.4
        ),
        temp_exp_cave = round(.data$temp_exp / ifelse(input$cave == 1, 3, 1), 3)) %>%
        select(.data$scenario_name, contains("MAT"), .data$temp_exp_cave) %>%
        rename_at(vars(contains("MAT")),
                  ~stringr::str_replace(.x, "MAT_", "Class ")) %>%
        rename(`Scenario Name` = .data$scenario_name,
               `Exposure Multiplier` = .data$temp_exp_cave)
    }, align = "r")

    output$cmd_tbl <- renderTable({
      req(!is.character(spat_res()))
      exp_df <-  spat_res() %>% rowwise() %>%
        mutate(moist_exp = case_when(
          CMD_6 >= 80 ~ 2,
          sum(CMD_6, CMD_5, na.rm = TRUE) >= 64 ~ 1.67,
          sum(CMD_6, CMD_5, CMD_4, na.rm = TRUE) >= 48 ~ 1.33,
          sum(CMD_6, CMD_5, CMD_4, CMD_3, na.rm = TRUE) >= 32 ~ 1,
          sum(CMD_6, CMD_5, CMD_4, CMD_3, CMD_2, na.rm = TRUE) >= 16 ~ 0.67,
          TRUE ~ 0.33
        ),
        moist_exp_cave = round(.data$moist_exp / ifelse(input$cave == 1, 3, 1), 3)) %>%
        select(.data$scenario_name, contains("CMD"), .data$moist_exp_cave) %>%
        rename_at(vars(contains("CMD")),
                  ~stringr::str_replace(.x, "CMD_", "Class ")) %>%
        rename(`Scenario Name` = .data$scenario_name,
               `Exposure Multiplier` = .data$moist_exp_cave)
    }, align = "r")

    output$tbl_ccei <- renderTable({
      req(!is.character(spat_res()))
      exp_df <-  spat_res() %>%
        select(.data$scenario_name,
               contains("CCEI", ignore.case = FALSE)) %>%
        rename_at(vars(contains("CCEI")),
                  ~stringr::str_replace(.x, "CCEI_", "Class ")) %>%
        rename(`Scenario Name` = .data$scenario_name)

    }, align = "r")

    # When next button is clicked move to next panel
    observeEvent(input$next2, {
      updateTabsetPanel(session, "tabset",
                        selected = "Exposure Results"
      )
      shinyjs::runjs("window.scrollTo(0, 0)")
    })
    # When next button is clicked move to next panel
    observeEvent(input$next3, {
      updateTabsetPanel(session, "tabset",
                        selected = "Vulnerability Questions"
      )
      shinyjs::runjs("window.scrollTo(0, 0)")
    })

    # Vulnerability Qs #===============
    # Show guidelines with additional info for each section
    observeEvent(input$guideB, {
      guideB()
    })

    observeEvent(input$guideC, {
      guideCNonSpatial()
    })

    observeEvent(input$guideD, {
      guideDNonSpatial()
    })

    # When next button is clicked move to next panel
    observeEvent(input$next4, {
      updateTabsetPanel(session, "tabset",
                        selected = "Spatial Vulnerability Questions")
      shinyjs::runjs("window.scrollTo(0, 0)")
    })

    # Spatial Vulnerability Questions #========================
    observeEvent(input$guideC2, {
      guideCSpatial()
    })

    observeEvent(input$guideD2, {
      guideDSpatial()
    })


    # C2ai
    observe({
      req(doSpatial())
      if(isTruthy(clim_vars()$htn)){
        shinyjs::hide("missing_C2ai")
        shinyjs::show("map_C2ai")
        shinyjs::show("not_missing_C2ai")
      } else {
        shinyjs::hide("map_C2ai")
        shinyjs::hide("not_missing_C2ai")
        shinyjs::show("missing_C2ai")
      }
    })

    output$map_C2ai <- tmap::renderTmap({
      req(doSpatial())
      req(clim_vars()$htn)

      make_map(isolate(range_poly_clim()), rast = clim_vars()$htn, rast_nm = "htn",
               rast_lbl = c("1 Low", "2", "3", "4 High"))
    })

    output$tbl_C2ai <- renderTable({
      exp_df <-  spat_res() %>%
        select(contains("HTN")) %>%
        rename_at(vars(contains("HTN")),
                  ~stringr::str_replace(.x, "HTN_", "Class ")) %>%
        tidyr::pivot_longer(cols = contains("Class"),
                     names_to = "Sensitivity Class", values_to = "Proportion of Range") %>%
        transmute(`Sensitivity Class` = stringr::str_replace(.data$`Sensitivity Class`, "Class 1", "Low - 1") %>%
                    stringr::str_replace("Class 4", "High - 4") %>%
                    stringr::str_remove("Class"), .data$`Proportion of Range`) %>%
        distinct()
    }, align = "r")

    output$box_C2ai <- renderUI({
      # get previous comment
      prevCom <- isolate(input$comC2ai)
      prevCom <- ifelse(is.null(prevCom), "", prevCom)
      box_val <- spat_res() %>%
        mutate(C2ai = case_when(HTN_1 > 10 ~ 0,
                                HTN_2 > 10 ~ 1,
                                HTN_3 > 10 ~ 2,
                                HTN_4 > 10 ~ 3,
                                is.na(HTN_1) ~ NA_real_)) %>%
        pull(.data$C2ai) %>% unique()

      check_comment_ui("C2ai", HTML("Calculated effect on vulnerability."),
                       choiceNames = valueNms,
                       choiceValues = valueOpts,
                       selected = box_val,
                       com = prevCom)
    })

    # This makes sure that the value is updated even if the tab isn't reopened
    outputOptions(output, "box_C2ai", suspendWhenHidden = FALSE)

    # C2aii
    observe({
      req(doSpatial())
      if(isTruthy(ptn_poly())){
        shinyjs::hide("missing_C2aii")
        shinyjs::show("not_missing_C2aii")
        shinyjs::show("map_C2aii")
      } else {
        shinyjs::hide("map_C2aii")
        shinyjs::hide("not_missing_C2aii")
        shinyjs::show("missing_C2aii")
      }
    })

    output$map_C2aii <- tmap::renderTmap({
      req(doSpatial())
      req(ptn_poly())

      make_map(poly1 = isolate(range_poly()), poly2 = ptn_poly(), poly2_nm = "ptn")
    })

    output$tbl_C2aii <- renderTable({
      exp_df <-  spat_res() %>%
        select(contains("PTN")) %>%
        tidyr::pivot_longer(cols = contains("PTN"),
                     names_to = "Variable", values_to = "Proportion of Range") %>%
        distinct()
    })

    output$box_C2aii <- renderUI({
      # get previous comment
      prevCom <- isolate(input$comC2aii)
      prevCom <- ifelse(is.null(prevCom), "", prevCom)
      box_val <- spat_res() %>%
        mutate(C2aii = case_when(PTN > 90 ~ 3,
                                 PTN > 50 ~ 2,
                                 PTN > 10 ~ 1,
                                 is.na(PTN) ~ NA_real_,
                                 TRUE ~ 0)) %>%
        pull(.data$C2aii) %>% unique()

      check_comment_ui("C2aii", HTML("Calculated effect on vulnerability."),
                       choiceNames = valueNms,
                       choiceValues = valueOpts,
                       selected = box_val,
                       com = prevCom)
    })

    # This makes sure that the value is updated even if the tab isn't reopened
    outputOptions(output, "box_C2aii", suspendWhenHidden = FALSE)

    # C2bi
    observe({
      req(doSpatial())
      if(isTruthy(clim_vars()$map)){
        shinyjs::hide("missing_C2bi")
        shinyjs::show("not_missing_C2bi")
        shinyjs::show("map_C2bi")
      } else {
        shinyjs::hide("map_C2bi")
        shinyjs::hide("not_missing_C2bi")
        shinyjs::show("missing_C2bi")
      }
    })

    output$map_C2bi <- tmap::renderTmap({
      req(doSpatial())
      req(clim_vars()$map)

      make_map(poly1 = isolate(range_poly_clim()), rast = clim_vars()$map, rast_nm = "map",
               rast_style = "pretty")
    })

    output$tbl_C2bi <- renderTable({
      exp_df <-  spat_res() %>%
        select(.data$MAP_max, .data$MAP_min) %>%
        rename(`Min MAP` = .data$MAP_min, `Max MAP` = .data$MAP_max) %>%
        distinct()
    })

    output$box_C2bi <- renderUI({
      # get previous comment
      prevCom <- isolate(input$comC2bi)
      prevCom <- ifelse(is.null(prevCom), "", prevCom)
      box_val <- spat_res() %>%
        mutate(range_MAP = .data$MAP_max - .data$MAP_min,
               C2bi = case_when(range_MAP < 100 ~ 3,
                                range_MAP < 254 ~ 2,
                                range_MAP < 508 ~ 1,
                                is.na(range_MAP) ~ NA_real_,
                                TRUE ~ 0)) %>%
        pull(.data$C2bi) %>% unique()

      check_comment_ui("C2bi", HTML("Calculated effect on vulnerability."),
                       choiceNames = valueNms,
                       choiceValues = valueOpts,
                       selected = box_val,
                       com = prevCom)
    })

    # This makes sure that the value is updated even if the tab isn't reopened
    outputOptions(output, "box_C2bi", suspendWhenHidden = FALSE)

    # D2 and D3
    observe({
      req(doSpatial())
      if(isTruthy(hs_rast())){
        shinyjs::hide("missing_D2_3")
        shinyjs::show("not_missing_D2_3")
        shinyjs::show("map_D2_3")
      } else {
        shinyjs::hide("map_D2_3")
        shinyjs::hide("not_missing_D2_3")
        shinyjs::show("missing_D2_3")
      }
    })

    # reclassify raster
    hs_rast2 <- reactive({
      req(hs_rast())
      rast <- raster::reclassify(hs_rast(),
                                 rcl = hs_rcl_mat(), right = NA)
    })

    output$map_D2_3 <- tmap::renderTmap({
      req(doSpatial())
      req(hs_rast2())

      make_map(poly1 = isolate(range_poly()), rast = hs_rast2(),
               poly2 = assess_poly(), poly2_nm = "assess_poly",
               rast_nm = "hs_rast",
               rast_lbl = data.frame(label = c("Not suitable", "Lost", "Maintained", "Gained"),
                                     value = c(0, 1, 2, 3)))
    })

    output$tbl_D2_3 <- renderTable({
      exp_df <-  spat_res() %>%
        select(`Scenario Name` = .data$scenario_name,
               `% Range Lost` = .data$range_change,
               `% Maintained` = .data$range_overlap)
    })

    output$box_D2 <- renderUI({
      # get previous comment
      prevCom <- isolate(input$comD2)
      prevCom <- ifelse(is.null(prevCom), "", prevCom)
      box_val <- spat_res() %>%
        mutate(D2 = case_when(range_change > 99 ~ 3,
                              range_change > 50 ~ 2,
                              range_change > 20 ~ 1,
                              is.na(range_change) ~ NA_real_,
                              TRUE ~ 0)) %>%
        pull(.data$D2)

      if(!is.null(hs_rast())){
        if(raster::nlayers(hs_rast2()) > 1){
          valueNm <- valueNms[ 4- box_val]
          div(strong("Calculated effect on vulnerability."),
              HTML("<font color=\"#FF0000\"><b> Spatial results can not be edited when multiple scenarios are provided.</b></font>"),
              HTML(paste0("<p>", clim_readme()$Scenario_Name, ": ", valueNm, "</p>")))

        } else {
          check_comment_ui("D3", HTML("Calculated effect on vulnerability."),
                           choiceNames = valueNms,
                           choiceValues = valueOpts,
                           selected = box_val,
                           com = prevCom)
        }
      } else {
        check_comment_ui("D2", HTML("Calculated effect on vulnerability."),
                         choiceNames = valueNms,
                         choiceValues = valueOpts,
                         selected = box_val,
                         com = prevCom)
      }
    })

    # This makes sure that the value is updated even if the tab isn't reopened
    outputOptions(output, "box_D2", suspendWhenHidden = FALSE)

    output$box_D3 <- renderUI({
      # get previous comment
      prevCom <- isolate(input$comD3)
      prevCom <- ifelse(is.null(prevCom), "", prevCom)
      box_val <- spat_res() %>%
        mutate(D2 = case_when(range_change > 99 ~ 3,
                              range_change > 50 ~ 2,
                              range_change > 20 ~ 1,
                              is.na(range_change) ~ NA_real_,
                              TRUE ~ 0),
               D3 = case_when(D2 == 3 ~ 0,
                              range_overlap == 0 ~ 3,
                              range_overlap < 30 ~ 2,
                              range_overlap < 60 ~ 1,
                              is.na(range_overlap) ~ NA_real_,
                              TRUE ~ 0)) %>%
        pull(.data$D3)

      if(!is.null(hs_rast())){
        if(raster::nlayers(hs_rast2()) > 1){
          valueNm <- valueNms[4 - box_val]
          div(strong("Calculated effect on vulnerability."),
              HTML("<font color=\"#FF0000\"><b> Spatial results can not be edited when multiple scenarios are provided.</b></font>"),
              HTML(paste0("<p>", clim_readme()$Scenario_Name, ": ", valueNm, "</p>")))

        } else {
          check_comment_ui("D3", HTML("Calculated effect on vulnerability."),
                           choiceNames = valueNms,
                           choiceValues = valueOpts,
                           selected = box_val,
                           com = prevCom)
        }
      } else {
        check_comment_ui("D3", HTML("Calculated effect on vulnerability."),
                         choiceNames = valueNms,
                         choiceValues = valueOpts,
                         selected = box_val,
                         com = prevCom)
      }
    })

    # This makes sure that the value is updated even if the tab isn't reopened
    outputOptions(output, "box_D3", suspendWhenHidden = FALSE)

    # When submit button is clicked move to next panel
    observeEvent(input$next5, {
      updateTabsetPanel(session, "tabset",
                        selected = "Index Results"
      )
      shinyjs::runjs("window.scrollTo(0, 0)")
    })

    # Calculate Index value #================================

    # Gather all the form inputs
    vuln_df <- eventReactive(input$calcIndex, {
      doSpatial()
        vuln_qs <- stringr::str_subset(names(input), "^[B,C,D]\\d.*")
        data <- purrr::map_df(vuln_qs, ~getMultValues(input[[.x]], .x))
        as_tibble(data)
    })

    # gather comments
    coms_df <- reactive({
      req(input$calcIndex)
      com_ins <- stringr::str_subset(names(input), "^com[B,C,D]\\d.*")

      data <- purrr::map_df(com_ins,
                            ~data.frame(Code = stringr::str_remove(.x, "com"),
                                        Comment = input[[.x]]))
    })

    index_res <- reactive({
      req(input$calcIndex)
      z_df <- data.frame(Code = c("Z2", "Z3"),
                         Value1 = as.numeric(c(input$cave, input$mig)))

      vuln_df <- bind_rows(vuln_df(), z_df) %>%
        mutate(Species = input$species_name)

      index <- calc_vulnerability(spat_res(), vuln_df, input$tax_grp)
      index
    })

    output$species_name <- renderText(input$species_name)

    # insert index dials for each scenario
    observeEvent(input$calcIndex, {
      removeUI(
        selector = "#*index_result*"
      )

      ind_ls <- index_res() %>% arrange(desc(scenario_name)) %>% split(index_res()$scenario_name)

      purrr::map(1:length(ind_ls),
                 ~insertUI(selector = paste0("#", "calcIndex"),
                           where = "afterEnd",
                           ui = indexOutUI(paste0("index_result",
                                                  .x))))

      purrr::map2(ind_ls, 1:length(ind_ls),
                  ~indexOutServer(paste0("index_result",
                                         .y),
                                  reactive(.x)))
    })

    # a flag to hide results until calculated
    output$calcFlag <- reactive(isTruthy(out_data()))
    outputOptions(output, "calcFlag", suspendWhenHidden = FALSE)

    output$n_factors <- renderTable({
      facts <- index_res() %>% distinct(across(contains("factors")))
      tibble(Section = c("Section B: Indirect Exposure to Climate Change",
                         "Section C: Sensitivity and Adaptive Capacity",
                         "Section D: Documented or Modeled Response to Climate Change"),
             `Factors completed` = c(paste0(facts$n_b_factors, "/4"),
                                     paste0(facts$n_c_factors, "/16"),
                                     paste0(facts$n_d_factors, "/4")))
    })

    output$slr <- renderText({
      if(!any(index_res()$slr_vuln)){
        return(NULL)
      }
      scn_slr <- filter(index_res(), slr_vuln) %>% pull(scenario_name)
      paste0("The index value for this species in scenario ",
             paste0(scn_slr, collapse = ", "), " was increased to ",
             "'Extremely Vulnerable' because it is vulnerable to rising ",
             "sea levels and has significant dispersal barriers")
    })

    output$ind_score_plt <- renderPlot({
      plot_score_index(index_res())
    })

    #output$conf_index <- renderText(index_res()$conf_index)
    output$conf_graph <- renderPlot({
      plot_conf_score(index_res())

    })

    output$q_score_plt <- plotly::renderPlotly({
      index_res() %>%
        select(scenario_name, vuln_df) %>%
        tidyr::unnest(vuln_df) %>%
        plot_q_score()
    })

    # Make csv
    out_data <- reactive({
      vuln_df <- purrr::map_dfr(index_res()$vuln_df, widen_vuln_coms,
                                coms_df = coms_df())

      spat_df <- spat_res()

      conf_df <- index_res() %>%
        select(scenario_name, mc_results) %>%
        mutate(mc_results = purrr::map(mc_results, ~.x$index %>%
                                         factor(levels = c( "EV", "HV", "MV", "LV", "IE")) %>%
                                         table() %>%
                                         prop.table() %>%
                                         as.data.frame(stringsAsFactors = FALSE) %>%
                                         `names<-`(c("index", "frequency")))) %>%
        pull(mc_results) %>%
        purrr::map_dfr(~ mutate(.x, index = paste0("MC_freq_", .data$index)) %>%
                                  tidyr::pivot_wider(names_from = "index",
                                                     values_from = "frequency"))

      data.frame(species_name = input$species_name,
                 common_name = input$common_name,
                 geo_location = input$geo_location,
                 assessor = input$assessor_name,
                 taxonomic_group = input$tax_grp,
                 migratory = input$mig,
                 cave_grnd_water = input$cave,
                 gain_mod = input$gain_mod,
                 gain_mod_comm = input$gain_mod_comm,
                 CCVI_index = index_res()$index,
                 CCVI_conf_index = index_res()$conf_index,
                 mig_exposure = index_res()$mig_exp,
                 b_c_score = index_res()$b_c_score,
                 d_score = index_res()$d_score) %>%
        bind_cols(conf_df, spat_df, vuln_df,
                  clim_readme() %>% select(-Scenario_Name)) %>%
        select(scenario_name, everything())
    })

    exportTestValues(out_data = out_data() %>% select(-contains("MC_freq")))

    # helpful for testing
    #shinyjs::runcodeServer()

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("CCVI_data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(out_data(), file, row.names = FALSE)
      }
    )

    output$downloadDefs <- downloadHandler(
      filename = "CCVI_column_definitions_results.csv",
      content = function(file) {
        out <- utils::read.csv(system.file("extdata/column_definitions_results.csv",
                                    package = "ccviR"))
        write.csv(out, file, row.names = FALSE)
      }
    )

    observeEvent(input$restart,{
      restoreURL <- paste0(session$clientData$url_protocol, "//",
                           session$clientData$url_hostname, ":",
                           session$clientData$url_port)

      # redirect user to restoreURL
      shinyjs::runjs(sprintf("window.location = '%s';", restoreURL))
    })

    # Bookmarking #=============================================================

    # this part is not allowed to be inside the module
    latestBookmarkURL <- reactiveVal()

    onBookmarked(
      fun = function(url) {
        latestBookmarkURL(parseQueryString(url))
        showNotification("Session saved",
                         duration = 10, type = "message")
      }
    )

    save_bookmark_server("save", latestBookmarkURL(), volumes)

    # Need to explicitly save and restore reactive values.
    onBookmark(fun = function(state){
      state$values$file_pths <- file_pths()
      state$values$clim_dir <- clim_dir_pth()
    })

    file_pths_restore <- reactiveVal()
    clim_dir_pth_restore <- reactiveVal()

    onRestore(fun = function(state){
      message("Restoring session")
      file_pths_restore(state$values$file_pths)
      clim_dir_pth_restore (state$values$clim_dir)
      restored$yes <- TRUE
    })

    # exclude shiny file choose and map and plotly input vals that might be
    # causing trouble
    # ExcludedIDs <- reactiveVal(value = NULL)
    # IncludedIDs <- reactiveVal(value = NULL)

    observe({
      patsToExclude <- paste0(c("plotly", "map", "pth", "data_prep", "dir",
                                "guide", "tabset", "next", "restart", "shinyalert"),
                              collapse = "|")

      toExclude <- grep(patsToExclude, names(input), value = TRUE)

      setBookmarkExclude(toExclude)
      # ExcludedIDs(toExclude)
      # IncludedIDs(setdiff(names(input), toExclude))
    })
#
#     output$ExcludedIDsOut <- renderText({
#       paste("ExcludedIDs:", paste(ExcludedIDs(), collapse = ", "))
#     })
#     output$IncludedIDsOut <- renderText({
#       paste("IncludedIDs:", paste(IncludedIDs(), collapse = ", "))
#     })

  }

  onStop(function(){options(testmode_in)})

  shinyApp(ui, server, enableBookmarking = "server",
           options = list(...))
}

