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
  clear <- actionButton(paste0(id, "_clear"), label = character(0), icon = icon("close"))

  return(div(label, button, splitLayout(text_out, clear, cellWidths = c("80%", "20%"))))
}

updateGet_file_ui <- function(inputId, value, ...){
  NULL
}

guide_popup <- function(id){
  id <- str_remove(id, "help_")

  sec <- stringr::str_extract(id, "^[B-D]")
  q <- stringr::str_extract(id, "\\d")
  q1 <- stringr::str_extract(id, "[a-h]")
  q2 <- stringr::str_extract(id, "i+")

  txt <- guideline_lu_tbl %>% filter(section == sec, question == q |is.na(question),
                              sub_question == q1 | is.na(sub_question),
                              sub2_question == q2 | is.na(sub2_question)) %>%
    pull(guide_text) %>% paste(collapse = "\n")

  ttl <- vulnq_code_lu_tbl %>% filter(Code == id) %>% pull(Question)

  showModal(modalDialog(title = ttl, HTML({txt})))
}


check_comment_ui <- function(id, label, com = "", ...){
  div(id = paste0(id, "div"),
      fluidRow(
        column(9, checkboxGroupInput(id, label, inline = TRUE, ...)),
        column(1, actionButton(paste0("help_", id), label = "", icon = icon("info")))
      ),
      #decrease whitespace b/w elements
      div(style = "margin-top: -1.5em"),
      textAreaInput(paste0("com", id), label = NULL, placeholder = "Comments",
                    value = com)
  )
}

updateCheck_comment_ui <- function(inputId, value, com, session){
  updateCheckboxGroupInput(session = session, inputId = inputId,
                           selected = as.integer(unlist(strsplit(value,","))))
  updateTextAreaInput(session = session, inputId = paste0("com", inputId),
                      value = ifelse(is.na(com), "", com))
}

spat_vuln_ui <- function(id, header = NULL, vuln_q_nm = NULL, chk_box = TRUE){
  tagList(div(
    id = paste0("div1_", id),
    if(!is.null(header)) {h4(header)},
    if(!is.null(vuln_q_nm)) {strong(vuln_q_nm)},
    br(),br(),
    div(id = paste0("missing_", id),
        HTML("<b>Spatial data not provided.</b> <br>Answer the questions below based on expert knowledge or leave blank for unknown."),
        br(),
        br()),
    leaflet::leafletOutput(paste0("map_", id)),
    tableOutput(paste0("tbl_", id)),
    div(id = paste0("not_missing_", id),
        HTML("<font color=\"#FF0000\"><b> Editing the response below will override the results of the spatial analysis.</b></font>")),
    if(chk_box){
      uiOutput(paste0("box_", id))
    }
  ))
}

updateSpat_vuln_ui <- function(inputId, value, com, session){
  # Could the spat_res object be recreated from the file...? If yes wouldn't
  # need to do this...except for the comment so probably better it comes from
  # the file rather than the spat_res object in case it has been changed.
  updateCheck_comment_ui(inputId, value, com, session)
}

from_to_ui <- function(id, header, vals){
  tagList(
    strong(header),
    tags$div(numericInput(paste0(id, "_from"), "From", vals[1]), style="display:inline-block"),
    tags$div(numericInput(paste0(id, "_to"), "To", vals[2]), style="display:inline-block"),
    br()
  )

}

updateFrom_to_ui <- function(inputId, vals, session){
  updateNumericInput(session = session, paste0(inputId, "_from"), value = vals[1])
  updateNumericInput(session = session, paste0(inputId, "_to"), value = vals[2])
}
