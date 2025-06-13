# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# Get file path
get_file_ui <- function(id, ui_id, title, mandatory = FALSE, type = "file",
                        subtitle = "", multiple = FALSE, spinner = FALSE,
                        note = NULL) {

  title2 <- strong(paste0(title, ": "))
  if(mandatory) title2 <- labelMandatory(title2)
  label <- span(title2, subtitle)

  text_out <- verbatimTextOutput(NS(id, paste0(ui_id, "_out")), placeholder = TRUE)
  error_out <- verbatimTextOutput(NS(id, paste0(ui_id, "_error")))

  if(spinner) {
    text_out <- shinycssloaders::withSpinner(text_out, proxy.height = "100px")
  }

  button <- switch(
    type,
    file = shinyFiles::shinyFilesButton(NS(id, ui_id), "Choose file",
                                        title, multiple = multiple),
    dir = shinyFiles::shinyDirButton(NS(id, ui_id), "Choose a folder",
                                     title, multiple = multiple)
  )

  clear <- actionButton(NS(id, paste0(ui_id, "_clear")), label = character(0), icon = icon("close"))

  div(label,
      button,
      if(!is.null(note)) tagList(br(), em(note)),
      splitLayout(text_out, clear, cellWidths = c("80%", "20%")),
      error_out
  )
}

updateGet_file_ui <- function(inputId, value, ...){
  NULL
}

guide_popup <- function(id){
  id <- stringr::str_remove(id, "help_")

  sec <- stringr::str_extract(id, "^[B-D]")
  q <- stringr::str_extract(id, "\\d")
  q1 <- stringr::str_extract(id, "[a-h]")
  q2 <- stringr::str_extract(id, "i+")

  txt <- guideline_lu_tbl %>% filter(.data$section == sec, .data$question == q |is.na(.data$question),
                              .data$sub_question == q1 | is.na(.data$sub_question),
                              .data$sub2_question == q2 | is.na(.data$sub2_question)) %>%
    pull(.data$guide_text) %>%
    c(paste0(
      "<p><i>These guidelines are taken from the ",
      a("NatureServe Guidelines",
        href = "https://www.natureserve.org/sites/default/files/guidelines_natureserveclimatechangevulnerabilityindex_r3.02_1_jun_2016.pdf",
        target="_blank"),
      ".", "</i></p>"
      )) %>%
    paste(collapse = "\n")

  ttl <- vulnq_code_lu_tbl %>% filter(.data$Code == id) %>% pull(.data$Question)

  showModal(modalDialog(title = ttl, HTML({txt})))
}


#' Add Question inputs
#'
#' Adds a Question input with (optionally) a help button, comment box, as well
#' as input for "type of evidence".
#'
#' @param id Character. Id of the Shiny Module.
#' @param ui_id Character. Identifier for this group of inputs.
#' @param label Character. Question label.
#' @param com Character. Previous comment value.
#' @param evi Character. Previous evidence value.
#' @param guide Logical. Whether or not to include a help or info button.
#' @param ... Arguments passed on to `checkboxGroupInput()`.
#'
#' @noRd

check_comment_ui <- function(id, ui_id, label, chk_label = NULL, com = "", evi = "",
                             spatial = FALSE, guide = TRUE, multi_stop = FALSE,
                             ...){

  chkbxIn <- checkboxGroupInput(
    NS(id, ui_id),
    label = chk_label,
    inline = TRUE, ...)

  if(guide & !multi_stop) {
    chkbxIn <- fluidRow(
      column(9, chkbxIn),
      column(1, div(actionButton(NS(id, paste0("help_", ui_id)), label = "", icon = icon("info")),
                    style = "position: absolute;top: 15px;"))
    )
  } else if(guide & multi_stop) {
    chk_label <- fluidRow(
      column(9, chk_label),
      column(1, div(actionButton(NS(id, paste0("help_", ui_id)), label = "", icon = icon("info")),
                    style = "position: absolute;top: 15px;")))
  }

  e_id <- NS(id, paste0("evi_", ui_id))
  e_ui <- selectInput(e_id, label = NULL,
                      choices = c("Type of Evidence" = "", valueEvi),
                      selected = evi, multiple = TRUE)
  c_id <- NS(id, paste0("com_", ui_id))
  c_ui <- textAreaInput(c_id, label = NULL,
                        placeholder = "Comments", value = com)

  div(
    if(!is.null(label)) q5(label),
    div(id = NS(id, paste0(ui_id, "div")),
        style = "margin-left: 1em; margin-top: -1.5em",
        if(!multi_stop) chkbxIn,
        if(multi_stop) chk_label,
        # decrease whitespace b/w elements
        div(style = "margin-top: -1.5em", e_ui,
            div(style = "margin-top: -1em", c_ui)
        )
    )
  )
}

updateCheck_comment_ui <- function(inputId, value, com, evi, session) {
  updateCheckboxGroupInput(session = session, inputId = inputId,
                           selected = as.integer(unlist(strsplit(value,","))))
  updateSelectInput(session = session, inputId = paste0("evi_", inputId),
                    selected = if_else(is.na(evi), "", evi))
  updateTextAreaInput(session = session, inputId = paste0("com_", inputId),
                      value = if_else(is.na(com), "", com))
}

#' Create UI for spatial outputs depending on presence of spatial data
#'
#' @param ... Spatial data required.
#' @param id Character. Shiny module id.
#' @param ui_id Character. The id for this group of UI elements. Must be the
#'   name of the question in spat_res (e.g., `C2ai`, `D4`).
#' @param desc Character. A single string describing the spatial data required.
#'   Only required for creating the map/table outputs.
#' @param spat_df Reactive data frame. Spatial results table passed from the
#' spatial module. Only required if creating questions.
#' @param input Shiny Input object. Only required if creating questions.
#' @param map_table Logical. Whether to create the ui holders for the map and
#'  table.
#' @param q Logical. Whether to create the ui holders for a question.
#' @param multi_stop Logical. Whether to prevent question overrides if there are
#'   multiple scenarios.
#'
#' @details
#' Uses `render_spat_vuln_box2()` to create and pre-fill the questions (which in
#' turn uses `check_comment_ui2()` to create the UI.
#'
#' Where several questions share the same map/table output (e.g., D2 and D3),
#' use several calls to `spat_vuln_ui2()`, the first with `map_table = TRUE`,
#' but `q = FALSE`, with a joint id (e.g., "D2D3"). Then as many calls as
#' questions, each with `map_table = FALSE` but `q = TRUE`, and using the
#' correct id for that question (e.g., "D2").
#'
#' @returns Shiny UI taglist of UI elements
#' @noRd
#'
#' @examplesIf interactive()
#' # Example from `mod_A_server()`
#'
#' output$ui_texp <- renderUI({
#'   spat_vuln_ui(range_poly(), clim_vars(),
#'                id = id, ui_id = "texp",
#'                desc = "Range Polygon and Prepared Climate Data")
#' })
#'
#' # Example from `mod_C_server()`
#'
#' output$ui_C2aii <- renderUI({
#'   spat_vuln_ui(range_poly(), ptn_poly(),
#'                id = id, ui_id = "C2aii",
#'                desc = "\"Range Polygon\" and \"Physiological Thermal Niche\"",
#'                spat_df = spat_res(), input = input, q = TRUE)
#' })

spat_vuln_ui <- function(
    ..., id, ui_id, desc = NULL, spat_df = NULL, input = NULL,
    map_table = TRUE, q = FALSE, multi_stop = FALSE, optional = FALSE) {

  # TODO: Here we assume that if you do not have maps/tables, you don't have
  #  spatial. Correct assumption?

  ready <- is_ready(list(...)) && # Any errors in ...?
    all(vapply(list(...), isTruthy, logical(1))) # Any NULLs?

  # Show maps and tables or let user know
  if(map_table) {
    if(ready) {
      t <- tagList(
        shinycssloaders::withSpinner(leaflet::leafletOutput(NS(id, paste0("map_", ui_id)))),
        gt::gt_output(NS(id, paste0("tbl_", ui_id)))
      )
    } else {
      # No spatial - Let them know
      style <- "margin-left: 1em; margin-bottom: 1em;"
      if(optional) {
        t <- tagList(
          div(
            style = style,
            span("Optional Spatial Data not provided",
                 class = "optional-spatial"), br(),
            span("To calculate, provide ", desc, " on the Spatial Data Analysis page")
          ))
      } else {
        t <- tagList(
          div(
            style = style,
            span("Required Spatial Data not provided.",
                 class = "shiny-output-error-validation"), br(),
            span("Run Spatial Data Analysis with ", desc, " first")
          ))
      }
    }
  } else t <- tagList()


  # If Questions, prepare
  if(q) {

    multi_stop <- length(spat_df$range_change) > 1 &
      all(!is.na(spat_df$range_change)) &
      multi_stop

    chk_label <- NULL

    if(ready) {

      if(!multi_stop) {
        chk_label <- span(
          strong("Calculated effect on vulnerability"),
          span("(Changing this will override spatial analysis)",
               style = "font-size: 90%",
               class = "shiny-output-error-validation"))
      } else {
        chk_label <- div(
          span("Vulnerability Responses cannot be edited when multiple scenarios are provided",
               class = "shiny-output-error-validation"),
          p(HTML(paste0(spat_df$scenario_name, ": ", valueNms[4 - spat_df[[ui_id]]]) %>%
                   paste0(collapse = "<br>"))),
          style = "margin-top: 1.75em;"
        )
      }
    } else {
      chk_label <- span(
        "Spatial data not provided, answer based on other evidence or leave blank for unknown",
        style = "color:dimgray;")
    }


    box <- render_spat_vuln_box(id, ui_id, spat_df, input, chk_label,
                                multi_stop = multi_stop, is_spatial = ready)

    if(!ready && !optional) box <- shinyjs::hidden(box)

    t <- tagList(t, box)
  }

  t
}

updateSpat_vuln_ui <- function(inputId, value, com, evi, session){
  # Could the spat_res object be recreated from the file...? If yes wouldn't
  # need to do this...except for the comment so probably better it comes from
  # the file rather than the spat_res object in case it has been changed.
  # TODO: Is this used at all...?
  updateCheck_comment_ui(inputId, value, com, evi, session)
}


from_to_ui <- function(id, ui_id, header, vals){
  tagList(
    strong(header),
    tags$div(numericInput(NS(id, paste0(ui_id, "_from")), "From", vals[1]), style="display:inline-block"),
    tags$div(numericInput(NS(id, paste0(ui_id, "_to")), "To", vals[2]), style="display:inline-block"),
    br()
  )
}

updateFrom_to_ui <- function(inputId, value, session){
  vals <- stringr::str_split_1(value, ", ?")
  updateNumericInput(session = session, paste0(inputId, "_from"), value = vals[1])
  updateNumericInput(session = session, paste0(inputId, "_to"), value = vals[2])
}

q5 <- function(...) {
 h5(..., class = "question")
}
