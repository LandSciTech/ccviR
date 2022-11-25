# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# Get file path
get_file_ui <- function(id, title, mandatory = FALSE, type = "file",
                        subtitle = "", multiple = FALSE, spinner = FALSE){
  title2 <- strong(paste0(title, ": "))
  if(mandatory){
    title2 <- labelMandatory(title2)
  }
  label <- span(title2, subtitle)

  text_out <- verbatimTextOutput(paste0(id, "_out"), placeholder = TRUE)
  if(spinner){
    text_out <- shinycssloaders::withSpinner(text_out, proxy.height = "100px")
  }

  button <- switch(type,
                   file = shinyFiles::shinyFilesButton(id, "Choose file",
                                                       title, multiple = multiple),
                   dir = shinyFiles::shinyDirButton(id, "Choose a folder",
                                                    title, multiple = multiple))

    return(div(label, button, text_out))
}

updateGet_file_ui <- function(id){
  #Not currently updating
  NULL
}

check_comment_ui <- function(id, label, com = "", ...){
  div(id = paste0(id, "div"),
      checkboxGroupInput(id, label, inline = TRUE, ...),
      #decrease whitespace b/w elements
      div(style = "margin-top: -1.5em"),
      textAreaInput(paste0("com", id), label = NULL, placeholder = "Comments",
                    value = com)
  )

}

updateCheck_comment_ui <- function(inputId, value, com){
  updateCheckboxGroupInput(inputId = inputId, selected = value)
  updateTextAreaInput(inputId = inputId, value = ifelse(is.na(com), "", com))
}

spat_vuln_ui <- function(id, header = NULL, vuln_q_nm = NULL, chk_box = TRUE){
  tagList(div(
    id = id,
    if(!is.null(header)) {h4(header)},
    if(!is.null(vuln_q_nm)) {strong(vuln_q_nm)},
    br(),br(),
    div(id = paste0("missing_", id),
        HTML("<b>Spatial data not provided.</b> <br>Answer the questions below based on expert knowledge or leave blank for unknown."),
        br(),
        br()),
    tmap::tmapOutput(paste0("map_", id)),
    tableOutput(paste0("tbl_", id)),
    div(id = paste0("not_missing_", id),
        HTML("<font color=\"#FF0000\"><b> Editing the response below will override the results of the spatial analysis.</b></font>")),
    if(chk_box){
      uiOutput(paste0("box_", id))
    }
  ))
}

updateSpat_vuln_ui <- function(inputId, value, com){
  # Could the spat_res object be recreated from the file...? If yes wouldn't
  # need to do this...except for the comment so probably better it comes from
  # the file rather than the spat_res object in case it has been changed.
  updateCheck_comment_ui(inputId, value, com)
}

from_to_ui <- function(id, header, vals){
  tagList(
    strong(header),
    tags$div(numericInput(paste0(id, "_from"), "From", vals[1]), style="display:inline-block"),
    tags$div(numericInput(paste0(id, "_to"), "To", vals[2]), style="display:inline-block"),
    br()
  )

}

updateFrom_to_ui <- function(inputId, vals){
  updateNumericInput(paste0(inputId, "_from"), value = vals[1])
  updateNumericInput(paste0(inputId, "_to"), value = vals[2])
}
