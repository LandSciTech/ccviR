# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# Get file path
get_file_ui <- function(id, title, mandatory = FALSE, type = "file",
                        subtitle = "", multiple = FALSE){
  if(mandatory){
    label <- span(labelMandatory(strong(paste0(title, ": "))), subtitle)
  } else {
    label <- span(strong(paste0(title, ": ")), subtitle)
  }
  if(type == "file"){
    return(div(label,
               shinyFiles::shinyFilesButton(id, "Choose file",
                                            title, multiple = multiple),
               verbatimTextOutput(paste0(id, "_out"), placeholder = TRUE)))
  } else if(type == "dir"){
    return(div(label,
               shinyFiles::shinyDirButton(id, "Choose a folder",
                                          title, multiple = multiple),
               verbatimTextOutput(paste0(id, "_out"), placeholder = TRUE)))
  }

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

from_to_ui <- function(header, id, vals){
  tagList(
    strong(header),
    tags$div(numericInput(paste0(id, "_from"), "From", vals[1]), style="display:inline-block"),
    tags$div(numericInput(paste0(id, "_to"), "To", vals[2]), style="display:inline-block"),
    br()
  )

}
