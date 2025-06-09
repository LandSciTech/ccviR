#' Create leaflet map
#'
#' Function to make maps
#'
#' @param poly1 Sf Polygon. Primary polygon to plot.
#' @param rast1 SpatRaster. Primary raster to plot.
#' @param poly2 Sf Polygon. Secondary polygon to plot.
#' @param rast2 SpatRaster. Secondary raster to plot.
#' @param poly1_nm Character. Identifier.
#' @param poly2_nm Character. Identifier.
#' @param rast1_nm Character. Identifier.
#' @param rast2_nm Character. Identifier.
#' @param rast1_lbl Character. Raster layer labels. Only for primary raster.
#' @param rast_grp Character. Raster layer groups.
#' @param max_cell Numeric. Maximum number of cells in a raster before
#'   aggregating for plotting.
#'
#' @returns Leaflet map
#' @noRd

make_map2 <- function(poly1, rast1 = NULL, poly2 = NULL, rast2 = NULL,
                      poly1_nm = "Current Range", poly2_nm = NULL,
                      rast1_nm = NULL, rast2_nm = NULL,
                      rast1_lbl = NULL,
                      rast_grp = NULL, max_cell = 5000000) {

  # Name of input data layers for mapping
  rast_nms <- list(`Temperature exposure class` = "mat",
                   `Historical precipitation (mm)` = "map",
                   `Moisture class` = "cmd",
                   `Climate change exposure index` = "ccei",
                   `Historical thermal niche` = "htn",
                   `Modeled range change` = "hs_rast",
                   `Protected areas` = "protected_rast")

  poly_nms <- list(`Assessment area`= "assess_poly",
                   `Non-breeding range` = "nonbreed_poly",
                   `Physiological thermal niche` = "ptn")

  if(!is.null(rast1_nm)) {
    if(rast1_nm == "hs_rast"){
      pal1 = c("grey", "#FF0000", "#FFC125", "#008000")
      brks = 0:3
      rast_vals <- terra::unique(rast1, incomparables = TRUE) %>% unlist() %>%
        unique()
      rast1_lbl <- bind_cols(rast1_lbl, pal = pal1) %>%
        filter(.data$value %in% rast_vals)
      pal1 <- rast1_lbl$pal
      col_tbl <- data.frame(value = rast1_lbl$value, col = pal1)
      for(l in 1:terra::nlyr(rast1)){
        terra::coltab(rast1, layer = l) <- col_tbl
      }
      rast1_lbl <- pull(rast1_lbl, .data$label)
    } else if(rast1_nm %in% c("cmd", "mat")) {
      pal1 = c("#FFF9CA", "#FEE697", "#FEC24D", "#F88B22", "#D85A09", "#A33803")
      rast1_lbl <- as.character(1:6)
      col_tbl <- data.frame(value = 1:6, col = pal1)
      for(l in 1:terra::nlyr(rast1)){
        terra::coltab(rast1, layer = l) <- col_tbl
      }
      # add descriptor to class label
      rast1_lbl[1] <- paste0(rast1_lbl[1], " - Low")
      rast1_lbl[length(rast1_lbl)] <- paste0(rast1_lbl[length(rast1_lbl)], " - High")
    } else if(rast1_nm %in% c("ccei", "htn")) {
      pal1 <- c("#FFF7BD", "#FECF66", "#F88B22", "#CC4C02")
      rast1_lbl <- as.character(1:4)
      # add descriptor to class label
      rast1_lbl[1] <- paste0(rast1_lbl[1], " - Low")
      rast1_lbl[length(rast1_lbl)] <- paste0(rast1_lbl[length(rast1_lbl)], " - High")
      col_tbl <- data.frame(value = 1:4, col = pal1)
      for(l in 1:terra::nlyr(rast1)){
        terra::coltab(rast1, layer = l) <- col_tbl
      }
    } else if(rast1_nm == "protected_rast") {
      if(rast2_nm != "hs_rast" || is.null(rast2)) {
        stop("need future ranges for plotting protected areas", call. = FALSE)
      }
      pal1 <- c("#3C7F3C")
      pal2 <- "#410E48"
      rast2 <- terra::subst(rast2, c(0, 1), NA) # Remove non-range values
    } else if(rast1_nm == "map"){
      rng_val <- terra::minmax(rast1)[,1]
      pal1 <- leaflet::colorNumeric("Blues", domain = rng_val, na.color = "#00000000")
    } else {
      stop("no match for rast1_nm")
    }
  } else{
    if(!is.null(rast1)){
      stop("rast1_nm must be provided if rast1 is not NULL", call. = FALSE)
    }
  }

  # tried adding a line break to legend but doesn't work in interactive map
  poly2_nm <- names(poly_nms)[which(poly_nms == poly2_nm)]
  rast1_nm <- names(rast_nms)[which(rast_nms == rast1_nm)]
  rast2_nm <- names(rast_nms)[which(rast_nms == rast2_nm)]

  rast1 <- prep_raster_map(rast1, rast1_nm, max_cell)
  rast2 <- prep_raster_map(rast2, rast2_nm, max_cell)

  rast_grp <- character(0)
  if(!is.null(rast1) && terra::nlyr(rast1) > 1) rast_grp <- names(rast1)
  if(!is.null(rast2) && terra::nlyr(rast2) > 1) rast_grp <- names(rast2)

  extra_pal <- NULL
  extra_labs <- NULL

  out <- leaflet::leaflet() %>%
    # Tiles first to go under rasters
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap") %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "CartoDB")

  # Add second Raster
  if(!is.null(rast2)) {
    for(l in 1:terra::nlyr(rast2)){
      out <- leaflet::addRasterImage(out, x = rast2[[l]], method = "ngb",
                                     colors = pal2,
                                     group = rast_grp[l], opacity = 1)
    }
    extra_pal <- c(extra_pal, pal2)
    extra_labs <- c(extra_labs, rast2_nm)
  }

  # Add primary Raster
  if(!is.null(rast1)) {
    for(l in 1:terra::nlyr(rast1)){
      out <- leaflet::addRasterImage(out, x = rast1[[l]], method = "ngb",
                                     colors = pal1,
                                     group = rast_grp[l], opacity = 1)
    }

    if(is.character(pal1)) { # If character palette (categorical)
      # If just one colour add to existing legend
      if(length(pal1) == 1) {
        if(is.null(rast1_lbl)) rast1_lbl <- rast1_nm
        extra_pal <- c(extra_pal, pal1)
        extra_labs <- c(extra_labs, rast1_lbl)
      } else {
        out <- leaflet::addLegend(out, colors = pal1, labels = rast1_lbl,
                                  title = rast1_nm, opacity = 1)
      }
    } else { # If palette function (continuous)
      out <- leaflet::addLegend(out, pal = pal1, values = rng_val[1]:rng_val[2],
                                title = rast1_nm, opacity = 1)
    }
  }

  # Add second polygon
  if(!is.null(poly2)) {
    out <- out %>%
      leaflet::addPolylines(data = poly2 %>% sf::st_transform(4326), color = "red")
    extra_pal <- c(extra_pal, "red")
    extra_labs <- c(extra_labs, poly2_nm)
  }

  # Add primary polygon and one-off legend elements
  extra_pal <- c(extra_pal, "black")
  extra_labs <- c(extra_labs, poly1_nm)
  out <- out %>%
    leaflet::addPolylines(data = poly1 %>% sf::st_transform(4326), color = "black") %>%
    leaflet::addLegend(colors = extra_pal, labels = extra_labs, opacity = 1)

  out <- out %>%
    leaflet::addLayersControl(
      baseGroups = c("CartoDB", "OpenStreetMap"),
      overlayGroups = rast_grp,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  return(out)
}


parse_path <- function(volumes, shiny_files_list) {
  req(!is.integer(shiny_files_list))
  parseFilePaths(volumes, shiny_files_list)$datapath
}

#' Loads a previously saved data set from a shinyFiles list
#'
#' @param path Character. File path
#'
#' @returns Loaded data frame
#' @noRd
#'
#' @examples
#' f <- parse_path(server_setup(), test_files("test_final.csv"))
#' load_previous(f)

load_previous <- function(path) {

  validate(need(fs::is_file(path) & fs::file_exists(path), "File doesn't exist"))

  df <- tryCatch(error = function(cnd) {
    validate(need(TRUE, "CSV file is empty, cannot restore from file."))
  },
  utils::read.csv(path, na.strings = "") # Only blanks are NA
  )

  validate(need(!(nrow(df) < 1 || !"scenario_name" %in% colnames(df)),
                "CSV file is empty, cannot restore from file."))
  return(df)
}


# Update UI based on values loaded from csv
update_restored2 <- function(df, session, section = NULL){
  # match column names to inputs and/or maybe reactive values?
  # will need some sort of lookup for what type of input needs to be updated

  # Catch comments
  df_coms <- df %>%
    select(matches("^(com|evi)_")) %>%
    distinct() %>%
    tidyr::pivot_longer(everything(), names_to = c("type", "input"),
                        names_pattern = "(com|evi)_(.+)",
                        values_to = "value",
                        values_transform = as.character) %>%
    tidyr::pivot_wider(names_from = "type", values_from = "value") %>%
    # Rename/Mutates like the following won't fail if evidence column doesn't exist
    rename_with(~stringr::str_replace_all(
      .x, c("com" = "comment", "evi" = "evidence"))) %>%
    mutate(across(any_of(c("comment", "evidence")), ~tidyr::replace_na(.x, "")))
  if(!"evidence" %in% names(df_coms)) df_coms$evidence <- ""
  df_coms <- df_coms %>%
    dplyr::mutate(evidence = stringr::str_split(.data$evidence, ", ?"))

  # Catch input values
  df2 <- df %>%
    select(-matches("^(com|evi)_")) %>%
    tidyr::pivot_longer(everything(), names_to = "input",
                        values_to = "value",
                        values_transform = as.character) %>%
    distinct() %>%
    mutate(input2 = ifelse(stringr::str_detect(.data$input, "rng_chg_pth"),
                           "rng_chg_pth", .data$input)) %>%
    left_join(df_coms, by = "input") %>%
    left_join(select(ui_build_table, "id", "section", "update_fun"),
              by = c("input2" = "id")) %>%
    select(-"input2") %>%
    filter(!is.na(.data$update_fun)) %>%
    mutate(
      comment = ifelse(
        is.na(.data$comment) & stringr::str_detect(.data$input, "^[B,C,D]\\d.*"),
        "", .data$comment),
      value = ifelse(is.na(.data$value) & stringr::str_detect(.data$input, "pth"),
                     "", .data$value)) %>%
    rowwise() %>%
    # TODO: Get rid of this when merging all versions
    mutate(update_fun = if_else(stringr::str_detect(.data$update_fun, "_"),
                                paste0(.data$update_fun, "2"), .data$update_fun)) %>%
    mutate(arg_name = intersect(c("selected", "value"),
                                methods::formalArgs(.data$update_fun)))

  # this is used as a trigger to skip running spatial until after returning to
  # UI so that input is updated with values from csv
  updateTextInput(session, inputId = "hidden", value = "yes")

  # Catch both "spatial" and "spatial_range_change" in "spatial" .env$section
  section2 <- stringr::str_extract(section, "[A-D]{1}$")
  section <- stringr::str_remove(section, "\\_[A-D]{1}$")
  df2 <- filter(df2,
                stringr::str_detect(.data$section, paste0("^", .env$section)),
                is.na(.env$section2) | stringr::str_detect(.data$input, .env$section2))

  df2 <- select(df2, -"section")

  # run the appropriate update function for each input
  # tricky part is supplying the right argument name for the update fun

  purrr::pwalk(df2, update_call2, session = session)
}

# build the call to update function from the inputs
update_call2 <- function(input, update_fun, value, arg_name, comment, evidence, session){

  update_fun <- get(update_fun)

  if(!is.na(comment)){
    if(arg_name == "value"){
      update_fun(session = session, inputId = input, value = value,
                 com = comment, evi = evidence)
    }
  } else {
    if(arg_name == "value"){
      value <- ifelse(value == "TRUE", TRUE, value)
      value <- ifelse(value == "FALSE", FALSE, value)
      update_fun(session = session, inputId = input, value = value)
    } else if(arg_name == "selected"){
      update_fun(session = session, inputId = input, selected = value)
    }
  }
}



spat_vuln_hide2 <- function(id, ...) {
  ready <- is_ready(list(...)) && # Any errors in ...?
    all(vapply(list(...), isTruthy, logical(1))) # Any NULLs?
  shinyjs::toggle(paste0(id, "div"), condition = ready)
}


# TODO: Move to app_utils_ui.R? Isn't this a UI creator?
render_spat_vuln_box2 <- function(id, ui_id, spat_df, input, chk_label = NULL,
                                  multi_stop = FALSE) {

  com_id <- paste0("com_", ui_id)
  evi_id <- paste0("evi_", ui_id)

  # get previous comment/evidence
  prevCom <- isolate(input[[com_id]])
  prevCom <- ifelse(is.null(prevCom), "", prevCom)
  prevEvi <- isolate(input[[evi_id]])
  prevEvi <- if(is.null(prevEvi)) "" else prevEvi # Permits lengths > 1

  # If have spatial analysis AND have data for this Question
  if(isTruthy(spat_df) && all(!is.na(spat_df[[ui_id]]) & spat_df[[ui_id]] > -1)) {
    box_val <- spat_df[[ui_id]] %>% unique()
    if(is.character(box_val)) box_val <- stringr::str_split_1(box_val, ", ?")
    if(all(prevEvi == "")) prevEvi <- "Spatial Analysis - ccviR"
  } else {
    box_val <- NULL
  }

  check_comment_ui2(id, ui_id, label = NULL, chk_label = chk_label,
                    choiceNames = valueNms,
                    choiceValues = valueOpts,
                    selected = box_val,
                    com = prevCom, evi = prevEvi,
                    spatial = TRUE, multi_stop = multi_stop)
}


widen_vuln_coms2 <- function(questions) {

  comments <- bind_elements(questions, "comments")
  evidence <- bind_elements(questions, "evidence")

  vuln_df <- bind_elements(questions, "questions") %>%
    select("Code", matches("Value\\d")) %>%
    filter(!.data$Code %in% c("Z2", "Z3")) %>%
    arrange(.data$Code) %>%
    mutate_all(as.character) %>%
    tidyr::unite("Value", "Value1":"Value4", na.rm = TRUE, sep = ", ") %>%
    left_join(comments, by = "Code") %>%
    left_join(evidence, by = "Code") %>%
    tidyr::pivot_wider(names_from = "Code",
                       values_from = c("com", "evi", "Value")) %>%
    rename_all(~stringr::str_remove(.x, "Value_"))

  select(vuln_df, order(colnames(vuln_df)))
}


# Compare Questions in the index to questions in the app to see if they
# are in synchrony.
index_match_qs <- function(questions, index) {
  qs <- select(questions, -contains("com_"), -contains("evi_"))
  index <- select(index, matches("^[BCD]{1}\\d{1}[a-z]{0,3}")) %>%
    distinct()

  # If they match TRUE else FALSE
  nrow(qs) == nrow(index) && ncol(qs) == ncol(index) && all(qs == index)
}


combine_outdata2 <- function(species_data, questions, spat_run, spat_res, index) {

  # Priority order is species, question inputs, spatial run, spatial restored
  # TODO: Check this, and test different combinations of loading data and changing
  #  questions

  out_dat <- species_data %>%
    bind_cols(questions[, !names(questions) %in% names(.)]) %>%
    bind_cols(spat_run[, !names(spat_run) %in% names(.)]) %>%
    bind_cols(spat_res[, !names(spat_res) %in% names(.)]) %>%
    dplyr::mutate(ccviR_version = utils::packageVersion("ccviR"))

  # If there is no index OR the index questions don't match the answered
  # questions, don't save the index.
  if(!is.null(index) && index_match_qs(questions, index)) {
    # Get the comments and evidence from the actual questions
    index <- select(index, -starts_with("com_"), -starts_with("evi_"))
    out_dat <- select(out_dat, -any_of(colnames(index))) %>%
      bind_cols(index)
  } else {
    # Remove potentially out-of-date index scores
    out_dat <- select(
      out_dat, -any_of(c(
        "CCVI_index", "CCVI_conf_index", "mig_exposure", "b_c_score", "d_score",
        "MC_freq_EV", "MC_freq_HV", "MC_freq_MV", "MC_freq_LV", "MC_freq_IE"))
    )
  }


  exp_cols <- fs::path_package("extdata", "column_definitions_results.csv",
                               package = "ccviR") %>%
    utils::read.csv()

  exp_nms <- exp_cols %>%
    filter(.data$Column.Name != "") %>%
    rowwise() %>%
    mutate(names_exp = case_when(
      stringr::str_detect(.data$Column.Name, "HTN|CCEI") ~
        paste0(stringr::str_remove(Column.Name, "#"), 1:4, collapse = ","),
      stringr::str_detect(.data$Column.Name, "MAT|CMD") ~
        paste0(stringr::str_remove(Column.Name, "#"), 1:6, collapse = ","),
      stringr::str_detect(.data$Column.Name, "HTN|CCEI") ~
        paste0(stringr::str_remove(Column.Name, "#"), 1:4, collapse = ","),
      stringr::str_detect(.data$Column.Name, "MC_freq") ~
        paste0(stringr::str_remove(.data$Column.Name, "\\*"),
               c("EV", "HV", "MV", "LV", "IE"), collapse = ","),
      stringr::str_detect(.data$Column.Name, "^[B,C,D]\\d.*") ~
        paste0("evi_", .data$Column.Name, ",", "com_", .data$Column.Name, ",", .data$Column.Name),
      stringr::str_detect(.data$Column.Name, "MAP") ~
        paste0(stringr::str_remove(.data$Column.Name, "max/min"), c("max", "min"), collapse = ","),
      TRUE ~ .data$Column.Name
    )) %>%
    tidyr::separate_rows("names_exp", sep = ",") %>%
    pull(.data$names_exp)

  out_dat <- select(out_dat, any_of(exp_nms), contains("rng_chg_pth"))

  # add in missing column names
  add_nms <- setdiff(exp_nms, colnames(out_dat))
  if(length(add_nms) > 0){
    template <- rep("", length.out = length(add_nms))
    names(template) <- add_nms
    template <- as.data.frame(as.list(template))

    out_dat <- out_dat %>% bind_rows(template) %>%
      slice(-n())
  }

  return(out_dat)
}
