
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
      pal <- pull(rast_lbl, .data$pal)
      rast_lbl <- pull(rast_lbl, .data$label)
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
  } else{
    if(!is.null(rast)){
      stop("rast_nm must be provided if rast is not NULL", call. = FALSE)
    }
  }

  # tried adding a line break to legend but doesn't work in interactive map
  poly2_nm <- names(poly_nms)[which(poly_nms == poly2_nm)]
  rast_nm <- names(rast_nms)[which(rast_nms == rast_nm)]

  if(!is.null(rast)){
    if(terra::nlyr(rast) == 1){
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
                      palette = pal, group = rast_grp, breaks = brks)+
      tmap::tm_shape(poly1, name = poly1_nm)+
      tmap::tm_borders(col = "black", lwd = 2)+
      tmap::tm_add_legend("fill", labels = c(poly1_nm),
                          col = c("black"))+
      tmap::tm_facets(as.layers = TRUE)
  } else if(is.null(rast)){
    out <- tmap::tm_shape(poly1, name = poly1_nm)+
      tmap::tm_borders(col = "black", lwd = 2)+
      tmap::tm_shape(poly2, name = poly2_nm)+
      tmap::tm_borders(col = "red")+
      tmap::tm_add_legend("fill", labels = c(poly1_nm, poly2_nm),
                          col = c("black", "red"))+
      tmap::tm_facets(as.layers = TRUE)
  } else {
    out <-  tmap::tm_shape(rast, name = rast_nm)+
      tmap::tm_raster(title = rast_nm, style = rast_style, labels = rast_lbl,
                      palette = pal, group = rast_grp, breaks = brks)+
      tmap::tm_shape(poly1, name = poly1_nm)+
      tmap::tm_borders(col = "black", lwd = 2)+
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
      'Link_to_source'
    )))

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
