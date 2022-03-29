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

check_comment_ui <- function(id, label, com = "", ...){
  div(id = paste0(id, "div"),
      checkboxGroupInput(id, label, inline = TRUE, ...),
      #decrease whitespace b/w elements
      div(style = "margin-top: -1.5em"),
      textAreaInput(paste0("com", id), label = NULL, placeholder = "Comments",
                    value = com)
  )

}

# format multiple values from checkbox
getMultValues <- function(x, nm){
  if(is.null(x)){
    x <- -1
  }
  x <- as.numeric(x)

  df <- data.frame(Code = nm, Value1 = x[1], Value2 = x[2], Value3 = x[3],
                   Value4 = x[4], stringsAsFactors = FALSE)

}


# function to make maps (Uses some external objects, could be improved)
make_map <- function(poly1, rast = NULL, poly2 = NULL,
                     poly1_nm = "Range", poly2_nm = NULL,
                     rast_nm = NULL, rast_style = "cat", rast_lbl = NULL){

  # Name of input data layers for mapping
  rast_nms <- list(Temperature = "mat",
                   Precipitation = "map",
                   Moisture = "cmd",
                   `Climate change exposure index` = "ccei",
                   `Historical thermal niche` = "htn",
                   `Modeled range change` = "hs_rast")

  poly_nms <- list(`Assessment area`= "assess_poly",
                   `Non-breeding range` = "nonbreed_poly",
                   `Physiological thermal niche` = "ptn")

  if(!is.null(rast_nm)){
    if(rast_nm == "hs_rast"){
      pal = c("grey", "#FF0000", "#FFC125", "#008000")
      brks = 0:3
    } else if(rast_nm %in% c("cmd", "mat")) {
      pal = c("#FFF9CA", "#FEE697", "#FEC24D", "#F88B22", "#D85A09", "#A33803")
      brks = 1:7
      rast_style = "fixed"
      rast_lbl = as.character(1:6)
    } else if(rast_nm %in% c("ccei", "htn")) {
      pal = c("#FFF7BD", "#FECF66", "#F88B22", "#CC4C02")
      brks = 1:5
      rast_style = "fixed"
      rast_lbl = as.character(1:4)
    } else {
      pal = NULL
      brks = NULL
    }
  }

  # tried adding a line break to legend but doesn't work in interactive map
  poly2_nm <- names(poly_nms)[which(poly_nms == poly2_nm)]
  rast_nm <- names(rast_nms)[which(rast_nms == rast_nm)]

  if(is.null(poly2)){
    out <-  tmap::tm_shape(rast)+
      tmap::tm_raster(title = rast_nm, style = rast_style, labels = rast_lbl,
                      palette = pal, breaks = brks)+
      tmap::tm_shape(poly1)+
      tmap::tm_borders()+
      tmap::tm_add_legend("fill", labels = c(poly1_nm),
                          col = c("black"))
  } else if(is.null(rast)){
    out <- tmap::tm_shape(poly1)+
      tmap::tm_borders()+
      tmap::tm_shape(poly2)+
      tmap::tm_borders(col = "red")+
      tmap::tm_add_legend("fill", labels = c(poly1_nm, poly2_nm),
                          col = c("black", "red"))
  } else {
    out <-  tmap::tm_shape(rast)+
      tmap::tm_raster(title = rast_nm, style = rast_style, labels = rast_lbl,
                      palette = pal, breaks = brks)+
      tmap::tm_shape(poly1)+
      tmap::tm_borders()+
      tmap::tm_shape(poly2)+
      tmap::tm_borders(col = "red")+
      tmap::tm_add_legend("fill", labels = c(poly1_nm, poly2_nm),
                          col = c("black", "red"))
  }
  return(out)
}
