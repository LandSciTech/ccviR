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
            a("on GitHub", href = "https://github.com/see24/ccviR", target="_blank"),
            HTML("&bull;"),
            a("NatureServe website", href = "https://www.natureserve.org/conservation-tools/climate-change-vulnerability-index", target="_blank"))
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
              a("NatureServe", href = "https://www.natureserve.org/conservation-tools/climate-change-vulnerability-index", target="_blank"),
              "that automates the spatial analysis needed to inform the index. ",
              "The app is based on version 3.02 of the NatureServe CCVI. ",
              "See the app ",
              a("tutorial", href = "https://landscitech.github.io/ccviR/articles/app_vignette.html", target="_blank"),
              "for a demonstration of how to use the app. ",
              "For detailed instructions on how to use the index and definitions ",
              "of the terms used below see the ",
              a("NatureServe Guidelines.", href = "https://www.natureserve.org/sites/default/files/guidelines_natureserveclimatechangevulnerabilityindex_r3.02_1_jun_2016.pdf", target="_blank"),
              "Required datasets are indicated with ", labelMandatory("a"), "."),
            h3("Preparing to use the app"),

            p(strong("Step 0: "),"The first time you use the app you can either",
              a("download", href = "https://drive.google.com/drive/folders/18mO5GDUmwi-nswhIAC36bmtYsvmqNQkH?usp=share_link", target="_blank"),
              "a pre-prepared climate data set used in the app or",
              " prepare your own using raw climate data and the ",
              a("data preparation app.", href = "https://landscitech.github.io/ccviR/articles/data_prep_vignette.html", target="_blank")),
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
            #load_bookmark_ui("load"),
            shinyFilesButton("loadcsv", "Select file", "Select file", multiple = FALSE),
            br(),
            strong("Download column definitions for saved data"),
            br(),
            downloadButton("downloadDefs", "Download csv"),
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
                get_file_ui("clim_var_dir", "Folder location of prepared climate data",
                            type = "dir", mandatory = TRUE, spinner = TRUE),
                verbatimTextOutput("clim_var_error", ),
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
                                 from_to_ui("lost", "Lost:",  c(-1, -1)),
                                 from_to_ui("maint", "Maintained:", c(0, 0)),
                                 from_to_ui("gain", "Gained:", c(1,1)),
                                 from_to_ui("ns", "Not Suitable:", c(99, 99)),
                                 br(),
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
              p("Exposure is determined by the change in temperature or moisture",
                " that is expected to occur in the future. The maps below are",
                " created by taking historical climate - future climate and then",
                " classifying the results into six categories using the median",
                " and 1/2 the interquartile range. Thus, negative values for",
                " temperature indicate warmer conditions (\u00B0C) and negative values for",
                " moisture (mm) indicate drier conditions."),
              div(
                id = "texp_map_div",
                h3("Temperature exposure"),
                shinycssloaders::withSpinner(leaflet::leafletOutput("texp_map")),
                tableOutput("texp_tbl")
              ),
              div(
                id = "cmd_map_div",
                h3("Moisture exposure"),
                leaflet::leafletOutput("cmd_map"),
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
                  leaflet::leafletOutput("ccei_map"),
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
                chk_box = FALSE
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
              br(),
              br(),
              actionButton("restart", "Assess another species",
                           class = "btn-primary"),
              br(),
              br(),
              downloadButton("report", "Generate report", class = "btn-primary"),

              # # helpful for testing
              # shinyjs::runcodeUI()
            )
          )
        )
      ),
      div(
        id = "footer",
        style = "float:right",
        br(), br(), br(), br(),
        shinySaveButton("downloadData", "Save progress", "Save app data as a csv file",
                       class = "btn-primary", icon = shiny::icon("save")),
        br(),
        br(),
        br())
    )
  }

  # Server #========================
  server <- function(input, output, session) {
    file_pths <- NULL

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

    observeEvent(input$start, {
      updateTabsetPanel(session, "tabset",
                        selected = "Species Information"
      )
      shinyjs::runjs("window.scrollTo(0, 0)")
    })

    # restore a previous session
    shinyFileChoose("loadcsv", root = volumes, input = input,
                    filetypes = "csv")

    index_res <- reactiveVal(FALSE)
    spat_res <- reactiveVal(FALSE)
    file_pths <- reactiveVal()
    clim_dir_pth <- reactiveVal()

    # Restore from saved file #=================================================
    restored_df <- eventReactive(input$loadcsv, {
      if(!is.integer(input$loadcsv)){
        df_loaded <- read.csv(parseFilePaths(volumes, input$loadcsv)$datapath)

        update_restored(df_loaded, session)

        if(!is.null(df_loaded$MAT_6)){
          df_spat <- apply_spat_tholds(df_loaded, df_loaded$cave)
          spat_res(df_spat)
        }

        index_res(recreate_index_res(df_loaded))

        loaded_pths <- df_loaded %>% slice(1) %>%
          select(contains("pth"), -any_of("clim_dir_pth")) %>%
          as.list()

        if(length(loaded_pths)>0){
          file_pths(loaded_pths)
        }

        clim_dir_pth(df_loaded %>% slice(1) %>% .$clim_dir_pth)

        updateTabsetPanel(session, "tabset",
                          selected = "Species Information"
        )
        shinyjs::runjs("window.scrollTo(0, 0)")

        return(TRUE)
      }
    })

    observeEvent(restored_df(), {
      showNotification("Successfully restored from file.", duration = 10)
    })

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

    toListen <- reactive({
      purrr::map(filePathIds(), \(x){input[[x]]})
    })

    observeEvent(toListen(),{
      pths_in <- file_pths()

      purrr::walk(filePathIds(),
                  \(x) if(!is.integer(input[[x]])){
                    pths_in[[x]] <<- parseFilePaths(volumes, input[[x]])$datapath
                  })

      file_pths(pths_in)
    })

    # clear output filepaths when x clicked
    toClear <- reactive({
      purrr::map(paste0(filePathIds(), "_clear"), \(x){input[[x]]})
    })

    observeEvent(toClear(),{
      buttonIds <- paste0(filePathIds(), "_clear")
      pths_in <- file_pths()
      purrr::walk(buttonIds,
                  \(x){ if(input[[x]] > 0){
                    fl_x <- stringr::str_extract(x, "(.*)(_clear)", group = 1)
                    pths_in[[fl_x]] <<- ""
                  }})

      file_pths(pths_in)
    }, ignoreInit = TRUE)


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
    })

    # update filePathIds based on selection for rng_chg
    filePathIds <- reactive({
      # File path ids to use with file choose
      fileIds <- c("range_poly_pth", "nonbreed_poly_pth", "assess_poly_pth", "ptn_poly_pth")
      names(fileIds) <- fileIds

      rng_chg_pths <- stringr::str_subset(names(input), "rng_chg_pth_\\d$|rng_chg_pth$")

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
    observeEvent(input$clim_var_dir,{
      if(is.integer(input$clim_var_dir)){
        if (isTRUE(getOption("shiny.testmode"))) {
          pth <- system.file("extdata/clim_files/processed", package = "ccviR")
        } else {
          return(NULL)
        }
      }  else {
        pth <- parseDirPath(volumes, input$clim_var_dir)
      }

      clim_dir_pth(pth)
    })

    observeEvent(input$clim_var_dir_clear, {
      clim_dir_pth(NULL)
    })

    # output file paths
    output$clim_var_dir_out <- renderText({
      clim_dir_pth()
    })

    observe({
      purrr::walk2(file_pths(), filePathIds()[names(file_pths())], ~{
        out_name <- paste0(.y, "_out")
        output[[out_name]] <- renderText({.x})
      })
    })

    # load spatial data
    clim_readme <- reactive({
      req(clim_dir_pth())
      if(!file.exists(fs::path(clim_dir_pth(), "climate_data_readme.csv"))){
        stop("The climate folder is missing the required climate_data_readme.csv file",
             call. = FALSE)
      }
      utils::read.csv(fs::path(clim_dir_pth(), "climate_data_readme.csv"),
                      check.names = FALSE)
    })

    clim_vars <- reactive({
      root_pth <- clim_dir_pth()

      req(root_pth)
      req(clim_readme)

      clim_vars <- try(get_clim_vars(root_pth, scenario_names = clim_readme()$Scenario_Name))

    })

    range_poly_in <- reactive({

      if (isTRUE(getOption("shiny.testmode"))) {
        sf::st_read(system.file("extdata/rng_poly.shp",
                                package = "ccviR"),
                    agr = "constant", quiet = TRUE)
      } else {
        sf::st_read(file_pths()$range_poly_pth,
                    agr = "constant", quiet = TRUE)
      }

    })

    nonbreed_poly <- reactive({
      if (isTRUE(getOption("shiny.testmode"))) {
        # not currently included in package
        # pth <- system.file("extdata/nonbreed_poly.shp",
        #                    package = "ccviR")
        pth <- file_pths()$nonbreed_poly_pth
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
                    agr = "constant", quiet = TRUE) %>%
          valid_or_error("assessment area polygon")
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
        pth <- system.file("extdata/rng_chg_45.tif",
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

        check_trim(terra::rast(pth))
      }


    })

    ptn_poly <- reactive({

      if (isTRUE(getOption("shiny.testmode"))) {
        pth <- system.file("extdata/PTN_poly.shp", package = "ccviR")
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
    repeatSpatial <- reactiveVal(FALSE)

    observe({
      if(!is.null(restored_df())){
        repeatSpatial(TRUE)
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
      if(!repeatSpatial()){
        shinyjs::click("shinyalert")
      }
    })

    observeEvent(input$shinyalert, {
      removeModal()
      if(input$shinyalert > 0){
        doSpatial(TRUE)
        repeatSpatial(TRUE)
      }
      shinyjs::runjs("window.scrollTo(0, document.body.scrollHeight)")
    })

    # run spatial calculations
    spat_res1 <- eventReactive(input$shinyalert, {
      req(doSpatial())
      req(clim_vars())
        tryCatch({
          analyze_spatial(range_poly = range_poly_in(),
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
    observe({
      req(doSpatial())
      req(!is.character(spat_res1()))
      spat_res(spat_res1()$spat_table)
    })

    output$clim_var_error <- renderText({
      if(inherits(clim_vars(), "try-error")){
        stop(conditionMessage(attr(clim_vars(), "condition")))
      }
    })

    output$spat_error <- renderText({
      if(inherits(hs_rast(), "try-error")){
        stop("Error in range change raster",
             conditionMessage(attr(hs_rast(), "condition")))
      }
      if(is.character(spat_res1())){
        stop(spat_res1(), call. = FALSE)
      } else {
        "Spatial analysis complete"
      }
    })

    # When next button is clicked move to next panel
    observeEvent(input$next2, {
      updateTabsetPanel(session, "tabset",
                        selected = "Exposure Results"
      )
      shinyjs::runjs("window.scrollTo(0, 0)")
    })

    # calculate exp multipliers and vuln Q values for spat
    spat_res2 <- reactiveVal(FALSE)
    observe({
      req(!is.character(spat_res()))
      req(spat_res())
      spat_res2(apply_spat_tholds(spat_res(), input$cave))

    })

    # Exposure maps #=========================================================
    output$texp_map <- leaflet::renderLeaflet({
      req(!is.character(spat_res()))
      req(doSpatial())

      make_map(range_poly(), clim_vars()$mat, rast_nm = "mat",
               rast_lbl = c("1 High", "2", "3","4", "5", "6 Low"))
    })

    output$cmd_map <- leaflet::renderLeaflet({
      req(!is.character(spat_res()))
      req(doSpatial())
      isolate(
        make_map(range_poly(), clim_vars()$cmd, rast_nm = "cmd",
                 rast_lbl = c("1 High", "2", "3","4", "5", "6 Low"))
      )
    })

    output$texp_tbl <- renderTable({
      req(spat_res2())
      class_brks <- clim_readme()$brks_mat %>% unique() %>%
        stringr::str_split_1(";") %>% sort()
      exp_df <-  spat_res2() %>% rowwise() %>%
        select("scenario_name", matches("MAT_\\d"), "temp_exp_cave") %>%
        purrr::set_names(c("Scenario Name", class_brks, "Exposure Multiplier"))
    }, align = "r")

    output$cmd_tbl <- renderTable({
      req(spat_res2())
      class_brks <- clim_readme()$brks_cmd %>% unique() %>%
        stringr::str_split_1(";") %>% sort()
      exp_df <-  spat_res2() %>% rowwise() %>%
        select("scenario_name", matches("CMD_\\d"), "moist_exp_cave") %>%
        purrr::set_names(c("Scenario Name", class_brks, "Exposure Multiplier"))
    }, align = "r")


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

    output$ccei_map <- leaflet::renderLeaflet({
      req(!is.character(spat_res()))
      req(doSpatial())
      req(clim_vars()$ccei)
      req(isolate(nonbreed_poly()))
      isolate(
        make_map(nonbreed_poly(), clim_vars()$ccei, rast_nm = "ccei",
                 rast_lbl = c("1 Low", "2", "3", "4 High"))
      )
    })

    output$tbl_ccei <- renderTable({
      req(spat_res2())
      if(is.null(clim_readme()$brks_ccei)){
        stop("climate readme file does not contain breaks for ccei")
      }
      class_brks <- clim_readme()$brks_ccei %>% unique() %>%
        stringr::str_split_1(";") %>% sort()
      exp_df <-  spat_res2() %>%
        select("scenario_name",
               contains("CCEI", ignore.case = FALSE)) %>%
        purrr::set_names(c("Scenario Name", class_brks, "Exposure Multiplier"))

    }, align = "r")

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
    observe({spat_vuln_hide("C2ai", clim_vars()$htn, doSpatial(), restored_df())})

    output$map_C2ai <- leaflet::renderLeaflet({
      req(doSpatial())
      req(clim_vars()$htn)

      make_map(isolate(range_poly_clim()), rast = clim_vars()$htn, rast_nm = "htn",
               rast_lbl = c("1 Low", "2", "3", "4 High"))
    })

    output$tbl_C2ai <- renderTable({
      req(spat_res2())
      exp_df <-  spat_res2() %>%
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

    # create reactive for value used to choose checkbox b/c can't depend on
    # spat_res2 directly or it never renders when restoring
    box_val <- reactiveVal()

    observe({
      if(isTruthy(spat_res2())){
        box_val(spat_res2())
      }
    })

    output$box_C2ai <- renderUI({
      render_spat_vuln_box("C2ai", box_val(), input, valueNms, valueOpts)
    })

    # This makes sure that the value is updated even if the tab isn't reopened
    outputOptions(output, "box_C2ai", suspendWhenHidden = FALSE)

    # C2aii
    observe({spat_vuln_hide("C2aii", ptn_poly(), doSpatial(), restored_df())})

    output$map_C2aii <- leaflet::renderLeaflet({
      req(doSpatial())
      req(ptn_poly())

      make_map(poly1 = isolate(range_poly()), poly2 = ptn_poly(), poly2_nm = "ptn")
    })

    output$tbl_C2aii <- renderTable({
      req(spat_res())
      exp_df <-  spat_res() %>%
        select(contains("PTN", ignore.case = FALSE)) %>%
        tidyr::pivot_longer(cols = contains("PTN", ignore.case = FALSE),
                     names_to = "Variable", values_to = "Proportion of Range") %>%
        distinct()
    })

    output$box_C2aii <- renderUI({
      render_spat_vuln_box("C2aii", box_val(), input, valueNms, valueOpts)
    })

    # This makes sure that the value is updated even if the tab isn't reopened
    outputOptions(output, "box_C2aii", suspendWhenHidden = FALSE)

    # C2bi
    observe({spat_vuln_hide("C2bi", clim_vars()$map, doSpatial(), restored_df())})

    output$map_C2bi <- leaflet::renderLeaflet({
      req(doSpatial())
      req(clim_vars()$map)

      make_map(poly1 = isolate(range_poly_clim()), rast = clim_vars()$map,
               rast_nm = "map")
    })

    output$tbl_C2bi <- renderTable({
      req(spat_res())
      exp_df <-  spat_res() %>%
        select("MAP_max", "MAP_min") %>%
        rename(`Min MAP` = .data$MAP_min, `Max MAP` = .data$MAP_max) %>%
        distinct()
    })

    output$box_C2bi <- renderUI({
      render_spat_vuln_box("C2bi", box_val(), input, valueNms, valueOpts)
    })

    # This makes sure that the value is updated even if the tab isn't reopened
    outputOptions(output, "box_C2bi", suspendWhenHidden = FALSE)

    # D2 and D3
    observe({
      spat_vuln_hide("D2_3", hs_rast(), doSpatial(), restored_df())
    })

    # reclassify raster
    hs_rast2 <- reactive({
      req(hs_rast())
      rast <- terra::classify(hs_rast(),
                                 rcl = hs_rcl_mat(), right = NA)
    })

    output$map_D2_3 <- leaflet::renderLeaflet({
      req(doSpatial())
      req(hs_rast2())

      make_map(poly1 = isolate(range_poly()), rast = hs_rast2(),
               poly2 = assess_poly(), poly2_nm = "assess_poly",
               rast_nm = "hs_rast",
               rast_lbl = data.frame(label = c("Not suitable", "Lost", "Maintained", "Gained"),
                                     value = c(0, 1, 2, 3)))
    })

    output$tbl_D2_3 <- renderTable({
      req(spat_res())
      exp_df <-  spat_res() %>%
        select(`Scenario Name` = .data$scenario_name,
               `% Range Lost` = .data$range_change,
               `% Maintained` = .data$range_overlap)
    })

    output$box_D2 <- renderUI({
      req(spat_res2())
      # get previous comment
      prevCom <- isolate(input$comD2)
      prevCom <- ifelse(is.null(prevCom), "", prevCom)
      box_val <- spat_res2() %>%
        pull(.data$D2)

      if(nrow(spat_res2()) > 1){
        valueNm <- valueNms[ 4- box_val]
        div(strong("Calculated effect on vulnerability."),
            HTML("<font color=\"#FF0000\"><b> Spatial results can not be edited when multiple scenarios are provided.</b></font>"),
            HTML(paste0("<p>", clim_readme()$Scenario_Name, ": ", valueNm, "</p>")))

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
      req(spat_res2())
      # get previous comment
      prevCom <- isolate(input$comD3)
      prevCom <- ifelse(is.null(prevCom), "", prevCom)
      box_val <- spat_res2() %>%
        pull(.data$D3)

      if(nrow(spat_res2()) > 1){
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
    vuln_df <- reactive({
      vuln_qs <- stringr::str_subset(names(input), "^[B,C,D]\\d.*")
      data <- purrr::map_df(vuln_qs, ~getMultValues(input[[.x]], .x))
      as_tibble(data)
    })

    # gather comments
    coms_df <- reactive({
      com_ins <- stringr::str_subset(names(input), "^com[B,C,D]\\d.*")

      data <- purrr::map_df(com_ins,
                            ~data.frame(Code = stringr::str_remove(.x, "com"),
                                        com = input[[.x]]))
    })

    observeEvent(input$calcIndex,{
      z_df <- data.frame(Code = c("Z2", "Z3"),
                         Value1 = as.numeric(c(input$cave, input$mig)))

      vuln_df <- bind_rows(vuln_df(), z_df) %>%
        mutate(Species = input$species_name)

      index <- calc_vulnerability(spat_res(), vuln_df, input$tax_grp)
      index_res(index)
    })

    output$species_name <- renderText(input$species_name)

    # insert index dials for each scenario
    observe({
      req(index_res())
      removeUI(
        selector = "#*index_result*"
      )

      ind_ls <- index_res() %>% arrange(desc(.data$scenario_name)) %>%
        split(index_res()$scenario_name)

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
    output$calcFlag <- reactive(isTruthy(index_res()))
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
      scn_slr <- filter(index_res(), .data$slr_vuln) %>% pull(.data$scenario_name)
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
        select("scenario_name", "vuln_df") %>%
        tidyr::unnest(.data$vuln_df) %>%
        plot_q_score()
    })

    # Make out_data #========================================================
    out_data_lst <- reactiveValues()

    observe({
      sp_dat <- data.frame(species_name = input$species_name,
                           common_name = input$common_name,
                           geo_location = input$geo_location,
                           assessor_name = input$assessor_name,
                           tax_grp = input$tax_grp,
                           mig = input$mig,
                           cave = input$cave)
      res_df <- widen_vuln_coms(vuln_df(), coms_df = coms_df())

      out_data_lst$start <- bind_cols(sp_dat, res_df) %>%
        mutate(ccviR_version = utils::packageVersion("ccviR"))
    })

    observeEvent(spat_res(), {
      req(spat_res())
      req(clim_readme())
      req(!is.null(file_pths()))

      message("spat out_data")
      spat_df <- spat_res() %>% mutate(gain_mod = input$gain_mod,
                                       gain_mod_comm = input$gain_mod_comm)
      clim_rdme <- clim_readme() %>% select(-"Scenario_Name", -contains("brks"))
      spat_fnms <- lapply(file_pths(), function(x) ifelse(is.null(x),"",x)) %>%
        as.data.frame() %>%
        mutate(clim_dir_pth = clim_dir_pth())
      out_data_lst$spat <- bind_cols(
        spat_df %>% select(-any_of(c(colnames(clim_rdme),
                                                 colnames(spat_fnms)))),
        clim_rdme, spat_fnms)
    })

    observeEvent(index_res(), {
      req(index_res())
      message("index out_data")
      vuln_df <- purrr::map_dfr(index_res()$vuln_df, widen_vuln_coms,
                                coms_df = coms_df())

      conf_df <- index_res() %>%
        select("scenario_name", "mc_results") %>%
        mutate(mc_results = purrr::map(.data$mc_results, ~.x$index %>%
                                         factor(levels = c( "EV", "HV", "MV", "LV", "IE")) %>%
                                         table() %>%
                                         prop.table() %>%
                                         as.data.frame(stringsAsFactors = FALSE) %>%
                                         `names<-`(c("index", "frequency")))) %>%
        pull(.data$mc_results) %>%
        purrr::map_dfr(~ mutate(.x, index = paste0("MC_freq_", .data$index)) %>%
                         tidyr::pivot_wider(names_from = "index",
                                            values_from = "frequency"))

      ind_df <- data.frame(CCVI_index = index_res()$index,
                           CCVI_conf_index = index_res()$conf_index,
                           mig_exposure = index_res()$mig_exp,
                           b_c_score = index_res()$b_c_score,
                           d_score = index_res()$d_score)

      out_data_lst$index <- bind_cols(ind_df, conf_df, vuln_df)
    })

    exportTestValues(out_data = shiny::reactiveValuesToList(out_data_lst))

    # # helpful for testing
    # shinyjs::runcodeServer()

    # save the data to a file
    shinyFileSave(input, "downloadData", root = volumes, filetypes = "csv")

    observeEvent(input$downloadData, {
      if(!is.integer(input$downloadData)){
        filename <- parseSavePath(roots = volumes, input$downloadData)$datapath
        if(!stringr::str_detect(filename, "\\.csv$")){
          filename <- paste0(filename, ".csv")
        }
        saveAttempt <- tryCatch({
          write.csv(combine_outdata(reactiveValuesToList(out_data_lst)), filename,
                  row.names = FALSE)},
          error = function(e){
            showModal(modalDialog(
              p("File could not be saved. Is it open?"),
              footer = tagList(
                actionButton("retry", "Retry"),
                modalButton("Cancel")
              ),
              title = "Error Permission Denied"))
            print(conditionMessage(e))
          })
      }
    })

    # Retry save if there was an error due to open file
    observeEvent(input$retry, {
      # doesn't quite work but better than nothing
      removeModal()
      shinyjs::click("downloadData")
    })

    output$downloadDefs <- downloadHandler(
      filename = "CCVI_column_definitions_results.csv",
      content = function(file) {
        out <- utils::read.csv(system.file("extdata/column_definitions_results.csv",
                                    package = "ccviR"))
        write.csv(out, file, row.names = FALSE)
      }
    )

    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.pdf",
      content = function(file) {
        withProgress(message = 'Report rendering in progress...', {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy(system.file("rmd/results_report.Rmd", package = "ccviR"),
                  tempReport, overwrite = TRUE)


        rng_report <- try(range_poly(), silent = TRUE)
        if(!isTruthy(rng_report)){
          rng_report <- range_poly_in()
        }

        # Set up parameters to pass to Rmd document
        params <- list(out_data = shiny::reactiveValuesToList(out_data_lst) %>%
                                       combine_outdata(),
                                     clim_vars = clim_vars(),
                                     scale_poly = assess_poly(),
                                     range_poly = rng_report)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = "report.pdf",
                          params = params,
                          envir = new.env(parent = globalenv()))
        file.copy(file.path(tempdir(), 'report.pdf'), file)
        })
      }
    )

    observeEvent(input$restart,{
      restoreURL <- paste0(session$clientData$url_protocol, "//",
                           session$clientData$url_hostname, ":",
                           session$clientData$url_port)

      # redirect user to restoreURL
      shinyjs::runjs(sprintf("window.location = '%s';", restoreURL))
    })

  }

  onStop(function(){options(testmode_in)})

  shinyApp(ui, server, enableBookmarking = "server",
           options = list(...))
}

