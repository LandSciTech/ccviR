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
                     rast_nm = NULL, rast_style = "cat",
                     rast_lbl = NULL, rast_grp = NULL){

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
    } else {
      pal = NULL
    }
  } else{
    if(!is.null(rast)){
      stop("rast_nm must be provided if rast is not NULL", call. = FALSE)
    }
  }

  # tried adding a line break to legend but doesn't work in interactive map
  poly2_nm <- names(poly_nms)[which(poly_nms == poly2_nm)]
  rast_nm <- names(rast_nms)[which(rast_nms == rast_nm)]

  if(!is.null(rast)){
    if(raster::nlayers(rast) == 1){
      rast_grp <- rast_nm
    } else {
      if(is.null(rast_grp)){
        rast_grp <- NA
      }
    }
  }


  if(is.null(poly2)){
    out <-  tmap::tm_shape(rast, name = rast_nm)+
      tmap::tm_raster(title = rast_nm, style = rast_style, labels = rast_lbl,
                      palette = pal, group = rast_grp)+
      tmap::tm_shape(poly1, name = poly1_nm)+
      tmap::tm_borders()+
      tmap::tm_add_legend("fill", labels = c(poly1_nm),
                          col = c("black"))+
      tmap::tm_facets(as.layers = TRUE)
  } else if(is.null(rast)){
    out <- tmap::tm_shape(poly1, name = poly1_nm)+
      tmap::tm_borders()+
      tmap::tm_shape(poly2, name = poly2_nm)+
      tmap::tm_borders(col = "red")+
      tmap::tm_add_legend("fill", labels = c(poly1_nm, poly2_nm),
                          col = c("black", "red"))+
      tmap::tm_facets(as.layers = TRUE)
  } else {
    out <-  tmap::tm_shape(rast, name = rast_nm)+
      tmap::tm_raster(title = rast_nm, style = rast_style, labels = rast_lbl,
                      palette = pal, group = rast_grp)+
      tmap::tm_shape(poly1, name = poly1_nm)+
      tmap::tm_borders()+
      tmap::tm_shape(poly2, name = poly2_nm)+
      tmap::tm_borders(col = "red")+
      tmap::tm_add_legend("fill", labels = c(poly1_nm, poly2_nm),
                          col = c("black", "red"))+
      tmap::tm_facets(as.layers = TRUE)
  }
  return(out)
}

# create html text for index result for multi scenario
index_res_text <- function(ind_freq){

  ind_freq <- ind_freq %>%
    mutate(col = case_when(index == "IE" ~ "grey",
                            index == "EV" ~ "red",
                            index == "HV" ~ "darkorange",
                            index == "MV" ~ "#FFC125",
                            index == "LV" ~ "green",
                            TRUE ~ "grey"),
           def = case_when(index == "IE" ~ "Information entered about the species' vulnerability is inadequate to calculate an indexex score.",
                            index == "EV" ~ "Abundance and/or range extent within geographical area assessed extremely likely to substantially decrease or disappear by 2050.",
                            index == "HV" ~ "Abundance and/or range extent within geographical area assessed likely to decrease significantly by 2050.",
                            index == "MV" ~ "Abundance and/or range extent within geographical area assessed likely to decrease by 2050.",
                            index == "LV" ~ "Available evidence does not suggest that abundance and/or range extent within the geographical area assessed will change (increase/decrease) substantially by 2050. Actual range boundaries may change.",
                            TRUE ~ ""),
           index = case_when(index == "IE" ~ "Insufficient Evidence",
                            index == "EV" ~ "Extremely Vulnerable",
                            index == "HV" ~ "Highly Vulnerable",
                            index == "MV" ~ "Moderately Vulnerable",
                            index == "LV" ~ "Less Vulnerable",
                            TRUE ~ "Insufficient Evidence"))


  paste0("<h4><font color=", ind_freq$col, "><b>", ind_freq$index,
         ": </b></font>", paste0(ind_freq$scenarios), "</h4>",
        "<p>", ind_freq$def, "</p>", collapse = " ")
}

mig_exp_text <- function(mig_freq){
  mig_freq <- mig_freq %>%
    mutate(  col = case_when(mig_exp == "N/A" ~ "grey",
                             mig_exp == "High" ~ "red",
                             mig_exp == "Moderate" ~ "darkorange",
                             mig_exp == "Low" ~ "green",
                             TRUE ~ "grey"))

  paste0("<font color=", mig_freq$col, "><b>", mig_freq$mig_exp, ": </b></font>",
         mig_freq$scenarios, collapse = " ")
}

widen_vuln_coms <- function(vuln_df, coms_df){
  vuln_df %>%
    select(.data$Code, matches("Value\\d")) %>%
    filter(!.data$Code %in% c("Z2", "Z3")) %>%
    arrange(.data$Code) %>%
    mutate_all(as.character) %>%
    tidyr::unite("Value", .data$Value1:.data$Value4, na.rm = TRUE, sep = ", ") %>%
    left_join(coms_df, by = "Code") %>%
    tidyr::pivot_wider(names_from = "Code",
                       values_from = c("Comment","Value")) %>%
    rename_all(~paste0(stringr::str_extract(.x, "[B,C,D]\\d.*"), "_",
                       stringr::str_extract(.x, "^.*(?=_)")) %>%
                 stringr::str_remove("_Value")) %>%
    select(order(colnames(.)))
}
