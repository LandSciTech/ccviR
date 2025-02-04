# Get file path
get_file_ui2 <- function(id, ui_id, title, mandatory = FALSE, type = "file",
                         subtitle = "", multiple = FALSE, spinner = FALSE) {

  title2 <- strong(paste0(title, ": "))
  if(mandatory) title2 <- labelMandatory(title2)
  label <- span(title2, subtitle)

  text_out <- verbatimTextOutput(NS(id, paste0(ui_id, "_out")), placeholder = TRUE)

  if(spinner){
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
      splitLayout(text_out, clear, cellWidths = c("80%", "20%"))
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

check_comment_ui2 <- function(id, ui_id, label, com = "", evi = "", guide = TRUE, ...){
  if(guide){
    chkbxIn <- fluidRow(
      column(9, checkboxGroupInput(NS(id, ui_id), label, inline = TRUE, ...)),
      column(1, actionButton(NS(id, paste0("help_", ui_id)), label = "", icon = icon("info")))
    )
  } else {
    chkbxIn <- checkboxGroupInput(NS(id, ui_id), label, inline = TRUE, ...)
  }

  div(id = NS(id, paste0(ui_id, "div")),
      chkbxIn,
      #decrease whitespace b/w elements
      div(style = "margin-top: -1.5em",
          # TODO: Finalize evidence types

          selectInput(NS(id, paste0("evi", ui_id)), label = NULL,
                      choices = c("Type of Evidence" = "",
                                  "Literature",
                                  "Expert Opinion",
                                  "Spatial Analysis",
                                  "Spatial Analysis - ccviR", "Other"),
                      selected = evi),
      div(style = "margin-top: -1em",
          textAreaInput(NS(id, paste0("com", ui_id)), label = NULL, placeholder = "Comments",
                        value = com))
      )
  )

}

updateCheck_comment_ui2 <- function(inputId, value, com, evi, session) {
  updateCheckboxGroupInput(session = session, inputId = inputId,
                           selected = as.integer(unlist(strsplit(value,","))))
  updateSelectInput(session = session, inputId = paste0("evi", inputId),
                    selected = if_else(is.na(evi), "", evi))
  updateTextAreaInput(session = session, inputId = paste0("com", inputId),
                      value = if_else(is.na(com), "", com))
}


spat_vuln_ui2 <- function(id, ui_id, header = NULL, vuln_q_nm = NULL, chk_box = TRUE){
  tagList(div(
    id = NS(id, paste0("div1_", ui_id)),
    if(!is.null(header)) {h4(header)},
    if(!is.null(vuln_q_nm)) {strong(vuln_q_nm)},
    br(),br(),
    div(id = NS(id, paste0("missing_", ui_id)),
        HTML("<b>Spatial data not provided.</b> <br>Answer the questions below based on expert knowledge or leave blank for unknown."),
        br(),
        br()),
    leaflet::leafletOutput(NS(id, paste0("map_", ui_id))),
    tableOutput(NS(id, paste0("tbl_", ui_id))),
    div(id = NS(id, paste0("not_missing_", ui_id)),
        HTML("<font color=\"#FF0000\"><b> Editing the response below will override the results of the spatial analysis.</b></font>")),
    if(chk_box){
      uiOutput(NS(id, paste0("box_", ui_id)))
    }
  ))
}

updateSpat_vuln_ui2 <- function(inputId, value, com, evi, session){
  # Could the spat_res object be recreated from the file...? If yes wouldn't
  # need to do this...except for the comment so probably better it comes from
  # the file rather than the spat_res object in case it has been changed.
  updateCheck_comment_ui2(inputId, value, com, evi, session)
}
