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
