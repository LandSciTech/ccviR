# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# Get file path
get_file_ui <- function(id, title, mandatory = FALSE){
  if(mandatory){
    label <- labelMandatory(strong(title, ": "))
  } else {
    label <- strong(title, ": ")
  }
  div(label,
      shinyFilesButton(id, "Choose file",
                       title, multiple = FALSE),
      verbatimTextOutput(id, placeholder = TRUE),
      br())
}

