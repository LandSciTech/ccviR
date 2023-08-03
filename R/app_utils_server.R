
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
                     rast_lbl = NULL, rast_grp = NULL,
                     max_cell = 5000000){

  # Name of input data layers for mapping
  rast_nms <- list(`Temperature class` = "mat",
                   `Historical precipitation (mm)` = "map",
                   `Moisture class` = "cmd",
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
      rast_vals <- terra::unique(rast, incomparables = TRUE) %>% unlist() %>%
        unique()
      rast_lbl <- bind_cols(rast_lbl, pal = pal) %>%
        filter(.data$value %in% rast_vals)
      pal <- rast_lbl$pal
      col_tbl <- data.frame(value = rast_lbl$value, col = pal)
      for(l in 1:terra::nlyr(rast)){
        terra::coltab(rast, layer = l) <- col_tbl
      }
      rast_lbl <- pull(rast_lbl, .data$label)
    } else if(rast_nm %in% c("cmd", "mat")) {
      pal = c("#FFF9CA", "#FEE697", "#FEC24D", "#F88B22", "#D85A09", "#A33803")
      rast_lbl = as.character(1:6)
      col_tbl <- data.frame(value = 1:6, col = pal)
      for(l in 1:terra::nlyr(rast)){
        terra::coltab(rast, layer = l) <- col_tbl
      }
      # add descriptor to class label
      rast_lbl[1] <- paste0(rast_lbl[1], " - Low")
      rast_lbl[length(rast_lbl)] <- paste0(rast_lbl[length(rast_lbl)], " - High")
    } else if(rast_nm %in% c("ccei", "htn")) {
      pal = c("#FFF7BD", "#FECF66", "#F88B22", "#CC4C02")
      rast_style = "fixed"
      rast_lbl = as.character(1:4)
      # add descriptor to class label
      rast_lbl[1] <- paste0(rast_lbl[1], " - Low")
      rast_lbl[length(rast_lbl)] <- paste0(rast_lbl[length(rast_lbl)], " - High")
      col_tbl <- data.frame(value = 1:4, col = pal)
      for(l in 1:terra::nlyr(rast)){
        terra::coltab(rast, layer = l) <- col_tbl
      }
    } else if(rast_nm == "map"){
      rng_val <- terra::minmax(rast)[,1]
      pal <- leaflet::colorNumeric("Blues", domain = rng_val, na.color = "#00000000")
    } else {
      stop("no match for rast_nm")
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
    rast_ncell <- terra::ncell(rast)

    if(rast_ncell > max_cell){
      fct <- sqrt(rast_ncell/max_cell) %>% ceiling()
      rast <- terra::aggregate(rast, fact = fct, fun = "modal", na.rm = TRUE)
      message("aggregating raster for faster plotting")
    }

    if(!terra::same.crs(rast, "EPSG:3857")){
      rast <- terra::project(rast, "EPSG:3857", method = "near")
      message("projecting raster for plotting")
    }

    if(terra::nlyr(rast) == 1){
      rast_grp <- rast_nm
    } else {
      if(is.null(rast_grp)){
        rast_grp <- names(rast)
      } else {
        stopifnot(length(rast_grp) == terra::nlyr(rast))
      }
    }
  }

  out <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap") %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "CartoDB")

  if(is.null(poly2)){
    for(l in 1:terra::nlyr(rast)){
      out <- leaflet::addRasterImage(out, x = rast[[l]], method = "ngb",
                                     colors = pal,
                                     group = rast_grp[l], opacity = 1)
    }
    out <- out %>%
      leaflet::addPolylines(data = poly1 %>% sf::st_transform(4326), color = "black")
    if(is.character(pal)){
      out <- leaflet::addLegend(out, colors = pal, labels = rast_lbl,
                                title = rast_nm, opacity = 1)
    } else {
      out <- leaflet::addLegend(out, pal = pal, values = rng_val[1]:rng_val[2],
                                title = rast_nm, opacity = 1)
    }
    out <- out %>%
      leaflet::addLegend(colors = c("black"), labels = c(poly1_nm),
                         opacity = 1) %>%
      leaflet::addLayersControl(
        baseGroups = c("CartoDB", "OpenStreetMap"),
        overlayGroups = rast_grp,
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
  } else if(is.null(rast)){
    out <- out %>%
      leaflet::addPolylines(data = poly2 %>% sf::st_transform(4326), color = "red") %>%
      leaflet::addPolylines(data = poly1 %>% sf::st_transform(4326), color = "black") %>%
      leaflet::addLegend(colors = c("red", "black"), labels = c(poly2_nm, poly1_nm),
                         opacity = 1) %>%
      leaflet::addLayersControl(
        baseGroups = c("CartoDB", "OpenStreetMap"),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
  } else {
    for(l in 1:terra::nlyr(rast)){
      out <- leaflet::addRasterImage(out, x = rast[[l]], method = "ngb",
                                   group = rast_grp[l], opacity = 1)
    }
    out <- out %>%
      leaflet::addPolylines(data = poly2 %>% sf::st_transform(4326), color = "red") %>%
      leaflet::addPolylines(data = poly1 %>% sf::st_transform(4326), color = "black") %>%
      leaflet::addLegend(colors = pal, labels = rast_lbl, title = rast_nm, opacity = 1) %>%
      leaflet::addLegend(colors = c("red", "black"), labels = c(poly2_nm, poly1_nm),
                         opacity = 1) %>%
      leaflet::addLayersControl(
        baseGroups = c("CartoDB", "OpenStreetMap"),
        overlayGroups = rast_grp,
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
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
           def = case_when(index == "IE" ~ "Information entered about the species' vulnerability is inadequate to calculate an index score.",
                            index == "EV" ~ "Abundance and/or range extent within geographical area assessed extremely likely to substantially decrease or disappear.",
                            index == "HV" ~ "Abundance and/or range extent within geographical area assessed likely to decrease significantly.",
                            index == "MV" ~ "Abundance and/or range extent within geographical area assessed likely to decrease.",
                            index == "LV" ~ "Available evidence does not suggest that abundance and/or range extent within the geographical area assessed will change (increase/decrease) substantially. Actual range boundaries may change.",
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
  vuln_df <- vuln_df %>%
    select("Code", matches("Value\\d")) %>%
    filter(!.data$Code %in% c("Z2", "Z3")) %>%
    arrange(.data$Code) %>%
    mutate_all(as.character) %>%
    tidyr::unite("Value", .data$Value1:.data$Value4, na.rm = TRUE, sep = ", ") %>%
    left_join(coms_df, by = "Code") %>%
    tidyr::pivot_wider(names_from = "Code",
                       values_from = c("com","Value")) %>%
    rename_all(~stringr::str_remove(.x, "Value_"))

  select(vuln_df, order(colnames(vuln_df)))
}

combine_outdata <- function(out_data_lst){
  if(!is.null(out_data_lst$index)){
    out_data_lst$start <- out_data_lst$start %>%
      select(-any_of(colnames(out_data_lst$index)))
    out_data_lst$spat <- out_data_lst$spat %>%
      select(-any_of(colnames(out_data_lst$index)), -any_of(colnames(out_data_lst$start)))
  }

  bind_cols(out_data_lst) %>%
    select(any_of(c(
      'scenario_name', 'species_name', 'common_name', 'geo_location', 'assessor_name',
      'tax_grp', 'mig', 'cave', 'CCVI_index',
      'CCVI_conf_index', 'mig_exposure', 'b_c_score', 'd_score',
      'MC_freq_EV', 'MC_freq_HV', 'MC_freq_MV', 'MC_freq_LV', 'MC_freq_IE',
      'MAT_1', 'MAT_2', 'MAT_3', 'MAT_4', 'MAT_5', 'MAT_6', 'CMD_1', 'CMD_2', 'CMD_3',
      'CMD_4', 'CMD_5', 'CMD_6', 'CCEI_1', 'CCEI_2', 'CCEI_3', 'CCEI_4',
      'prop_non_breed_over_ccei', 'HTN_1', 'HTN_2', 'HTN_3', 'HTN_4', 'PTN',
      'MAP_max', 'MAP_min', 'range_change', 'range_overlap', 'range_size',
      'gain_mod', 'gain_mod_comm',
      'B1', 'com_B1', 'B2a', 'com_B2a', 'B2b', 'com_B2b', 'B3', 'com_B3',
      'C1', 'com_C1', 'C2ai', 'com_C2ai', 'C2aii', 'com_C2aii', 'C2bi',
      'com_C2bi', 'C2bii', 'com_C2bii', 'C2c', 'com_C2c', 'C2d',
      'com_C2d', 'C3', 'com_C3', 'C4a', 'com_C4a', 'C4b', 'com_C4b',
      'C4c', 'com_C4c', 'C4d', 'com_C4d', 'C4e', 'com_C4e', 'C4f',
      'com_C4f', 'C4g', 'com_C4g', 'C5a', 'com_C5a', 'C5b', 'com_C5b',
      'C5c', 'com_C5c', 'C6', 'com_C6', 'D1', 'com_D1', 'D2', 'com_D2',
      'D3', 'com_D3', 'D4', 'com_D4', 'GCM_or_Ensemble_name',
      'Historical_normal_period', 'Future_period', 'Emissions_scenario',
      'Link_to_source', 'range_poly_pth', 'nonbreed_poly_pth', 'assess_poly_pth',
      'ptn_poly_pth', 'clim_dir_pth'
    )), contains("rng_chg_pth"))

}

# read.csv("../../../Downloads/CCVI_data-2022-11-18 (1).csv") %>% colnames() %>% paste0(collapse = "', '")

# Update UI based on values loaded from csv
update_restored <- function(df, session){
  # match column names to inputs and/or maybe reactive values?
  # will need some sort of lookup for what type of input needs to be updated
  df_coms <- df %>% select(matches("^com_")) %>%
    tidyr::pivot_longer(everything(), names_to = "input",
                        names_prefix = "com_",
                        values_to = "comment",
                        values_transform = as.character) %>%
    mutate(comment = ifelse(is.na(comment), "", comment))

  df2 <- df %>% select(-matches("^com_")) %>%
    tidyr::pivot_longer(everything(), names_to = "input",
                             values_to = "value",
                             values_transform = as.character) %>%
    left_join(df_coms, by = "input") %>%
    left_join( ui_build_table %>% select(id, .data$update_fun), by = c("input" = "id")) %>%
    filter(!is.na(.data$update_fun)) %>%
    rowwise() %>%
    mutate(arg_name = intersect( c("selected", "value"), formalArgs(.data$update_fun)))

  # run the appropriate update function for each input
  # tricky part is supplying the right argument name for the update fun

  purrr::pwalk(df2, update_call, session = session)
}

# build the call to update function from the inputs
update_call <- function(input, update_fun, value, arg_name, comment, session){
  update_fun <- get(update_fun)
  if(!is.na(comment)){
    if(arg_name == "value"){
      update_fun(session = session, inputId = input, value = value, com = comment)
    }
  } else {
    if(arg_name == "value"){
      update_fun(session = session, inputId = input, value = value)
    } else if(arg_name == "selected"){
      update_fun(session = session, inputId = input, selected = value)
    }
  }
}

spat_vuln_hide <- function(id, check_exists, do_spat, restored){
  mis <- paste0("missing_", id)
  mapid <- paste0("map_", id)
  nmis <- paste0("not_missing", id)

  if(isTruthy(do_spat)){
    if(isTruthy(check_exists)){
      shinyjs::hide(mis)
      shinyjs::show(mapid)
      shinyjs::show(nmis)
    } else {
      shinyjs::hide(mapid)
      shinyjs::hide(nmis)
      shinyjs::show(mis)
    }
  } else if(isTruthy(restored)){
    shinyjs::hide(mis)
    shinyjs::hide(mapid)
    shinyjs::show(nmis)
  }
}

render_spat_vuln_box <- function(id, spat_df, input, valueNms, valueOpts){
  com_id <- paste0("com", id)
  # get previous comment
  prevCom <- isolate(input[[com_id]])
  prevCom <- ifelse(is.null(prevCom), "", prevCom)

  if(isTruthy(spat_df)){
    box_val <- spat_df[[id]] %>% unique()
  } else {
    box_val <- NULL
  }

  check_comment_ui(id, HTML("Calculated effect on vulnerability."),
                   choiceNames = valueNms,
                   choiceValues = valueOpts,
                   selected = box_val,
                   com = prevCom)
}

# recreate index_res object on loading from csv
recreate_index_res <- function(df){
  if(is.null(df$CCVI_index)){
    return(NULL)
  }

  spat_res <- apply_spat_tholds(df, df$cave)
  spat_res <- split(spat_res, spat_res$scenario_name)

  index_res <- df %>%
    select("scenario_name", index = .data$CCVI_index,
           conf_index = .data$CCVI_conf_index,
           contains("MC_freq"),
           mig_exp = .data$mig_exposure, .data$b_c_score, .data$d_score,
           -starts_with("com"),
           matches("^[B,C,D]\\d.*")) %>%
    mutate(across(matches("^[B,C,D]\\d.*"), as.character)) %>%
    tidyr::pivot_longer(matches("^[B,C,D]\\d.*"), names_to = "Code",
                        values_to = "Value") %>%
    tidyr::separate(.data$Value, into = (paste0("Value", 1:4)), fill = "right",
                    sep = ", ", convert = TRUE) %>%
    tidyr::nest(vuln_df = c(.data$Code, contains("Value"))) %>%
    mutate(vuln_df = purrr::map2(.data$vuln_df, spat_res, ~calc_vuln_score(.x, .y))) %>%
    tidyr::pivot_longer(contains("MC_freq"), names_to = "mc_index",
                        names_prefix = "MC_freq_",
                        values_to = "prop") %>%
    tidyr::nest(mc_results = c(.data$mc_index, .data$prop)) %>%
    mutate(mc_results = purrr::map(
      .data$mc_results,
      ~.x %>% rowwise() %>%
        mutate(index = list(rep(mc_index, 1000*prop))) %>%
        tidyr::unnest(cols = c(index)) %>%
        transmute(round_id = 1:n(), index, d_score = NA, b_c_score = NA)
    )) %>%
    mutate(n_b_factors = purrr::map_dbl(.data$vuln_df, get_n_factors, "B"),
           n_c_factors = purrr::map_dbl(.data$vuln_df, get_n_factors, "C"),
           n_d_factors = purrr::map_dbl(.data$vuln_df, get_n_factors, "D"))

  return(index_res)

}
