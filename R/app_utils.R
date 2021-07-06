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
      shinyFiles::shinyFilesButton(id, "Choose file",
                       title, multiple = FALSE),
      verbatimTextOutput(id, placeholder = TRUE),
      br())
}

# output file paths
file_pth_txt <- function(volumes, in_pth){
  renderText({
    shinyFiles::parseFilePaths(volumes, in_pth)$datapath
  })
}
