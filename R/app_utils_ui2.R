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

from_to_ui2 <- function(id, ui_id, header, vals){
  tagList(
    strong(header),
    tags$div(numericInput(NS(id, paste0(ui_id, "_from")), "From", vals[1]), style="display:inline-block"),
    tags$div(numericInput(NS(id, paste0(ui_id, "_to")), "To", vals[2]), style="display:inline-block"),
    br()
  )

}


check_comment_ui2 <- function(id, ui_id, label, com = "", guide = TRUE, ...){
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
      div(style = "margin-top: -1.5em"),
      textAreaInput(NS(id, paste0("com", ui_id)), label = NULL, placeholder = "Comments",
                    value = com)
  )
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
