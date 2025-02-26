# Get file path
get_file_ui2 <- function(id, ui_id, title, mandatory = FALSE, type = "file",
                         subtitle = "", multiple = FALSE, spinner = FALSE) {

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
      splitLayout(text_out, clear, cellWidths = c("80%", "20%")),
      error_out
  )
}

updateGet_file_ui2 <- function(inputId, value, ...){
  NULL
}

from_to_ui2 <- function(id, ui_id, header, vals){
  tagList(
    strong(header),
    tags$div(numericInput(NS(id, paste0(ui_id, "_from")), "From", vals[1]), style="display:inline-block"),
    tags$div(numericInput(NS(id, paste0(ui_id, "_to")), "To", vals[2]), style="display:inline-block"),
    br()
  )
}

updateFrom_to_ui2 <- function(inputId, value, session){
  vals <- as.integer(unlist(strsplit(value,",")))
  updateNumericInput(session = session, paste0(inputId, "_from"), value = vals[1])
  updateNumericInput(session = session, paste0(inputId, "_to"), value = vals[2])
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

check_comment_ui2 <- function(id, ui_id, label, chk_label = NULL, com = "", evi = "",
                              spatial = FALSE, guide = TRUE, ...){

  chkbxIn <- checkboxGroupInput(
    NS(id, ui_id),
    label = chk_label,
    inline = TRUE, ...)

  if(guide) {
    chkbxIn <- fluidRow(
      column(9, chkbxIn),
      column(1, div(actionButton(NS(id, paste0("help_", ui_id)), label = "", icon = icon("info")),
                    style = "position: absolute;top: 15px;"))
    )
  }

  div(
    if(!is.null(label)) q5(label),
    div(id = NS(id, paste0(ui_id, "div")),
        style = "margin-left: 1em; margin-top: -1.5em",
        chkbxIn,
        #decrease whitespace b/w elements
        div(style = "margin-top: -1.5em",
            # TODO: Finalize evidence types
            selectInput(NS(id, paste0("evi_", ui_id)), label = NULL,
                        choices = c("Type of Evidence" = "", valueEvi),
                        selected = evi),
            div(style = "margin-top: -1em",
                textAreaInput(NS(id, paste0("com_", ui_id)), label = NULL,
                              placeholder = "Comments", value = com))
        )
    )
  )
}

updateCheck_comment_ui2 <- function(inputId, value, com, evi, session) {
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
#'   spat_vuln_ui2(range_poly(), clim_vars(),
#'                id = id, ui_id = "texp",
#'                desc = "Range Polygon and Prepared Climate Data")
#' })
#'
#' # Example from `mod_C_server()`
#'
#' output$ui_C2aii <- renderUI({
#'   spat_vuln_ui2(range_poly(), ptn_poly(),
#'                id = id, ui_id = "C2aii",
#'                desc = "\"Range Polygon\" and \"Physiological Thermal Niche\"",
#'                spat_df = spat_res(), input = input, q = TRUE)
#' })

spat_vuln_ui2 <- function(..., id, ui_id, desc = NULL, spat_df = NULL, input = NULL,
                       map_table = TRUE, q = FALSE, multi_stop = FALSE) {

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
      t <- tagList(
        div(style = "margin-left: 1em; margin-bottom: 1em;",
            span("Cannot calculate values: Spatial Data not provided",
                 class = "shiny-output-error-validation"),
            br(),
            span("Require ", desc, "- See 'Spatial Data Analysis'."))
      )
    }
  } else t <- tagList()


  # If Questions, prepare
  if(q) {

    multi_stop <- length(spat_df$range_change) > 1 &
      all(!is.na(spat_df$range_change)) &
      multi_stop

    if(ready) {

      if(!multi_stop) {
        chk_label <- span(
          strong("Calculated effect on vulnerability"),
          span("(Changing this will override spatial analysis)",
               style = "font-size: 90%",
               class = "shiny-output-error-validation"))
      } else {
        chk_label <- span(
          strong("These results cannot be edited when multiple ",
                 "scenarios are provided"))
      }
    } else {
      chk_label <- span("Answer spatial question based on expert knowledge or ",
                        "leave blank for unknown.")
    }

    t <- tagList(
      t,
      render_spat_vuln_box2(id, ui_id, spat_df, input, chk_label,
                            multi_stop = multi_stop))
  }

  t
}



updateSpat_vuln_ui2 <- function(inputId, value, com, evi, session){
  # Could the spat_res object be recreated from the file...? If yes wouldn't
  # need to do this...except for the comment so probably better it comes from
  # the file rather than the spat_res object in case it has been changed.
  updateCheck_comment_ui2(inputId, value, com, evi, session)
}
