
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
      brks = 0:3
      rast_lbl <- bind_cols(rast_lbl, pal = pal) %>%
        filter(.data$value %in% raster::unique(rast))
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
                 stringr::str_remove("_Value"))

  select(vuln_df, order(colnames(vuln_df)))
}

combine_outdata <- function(out_data_lst){
  if(!is.null(out_data_lst$index)){
    out_data_lst$start <- out_data_lst$start %>%
      select(-any_of(colnames(out_data_lst$index)))
  }

  bind_cols(out_data_lst) %>%
    select(any_of(c(
      'scenario_name', 'species_name', 'common_name', 'geo_location', 'assessor',
      'taxonomic_group', 'migratory', 'cave_grnd_water', 'CCVI_index',
      'CCVI_conf_index', 'mig_exposure', 'b_c_score', 'd_score',
      'MC_freq_EV', 'MC_freq_HV', 'MC_freq_MV', 'MC_freq_LV', 'MC_freq_IE',
      'MAT_1', 'MAT_2', 'MAT_3', 'MAT_4', 'MAT_5', 'MAT_6', 'CMD_1', 'CMD_2', 'CMD_3',
      'CMD_4', 'CMD_5', 'CMD_6', 'CCEI_1', 'CCEI_2', 'CCEI_3', 'CCEI_4',
      'prop_non_breed_over_ccei', 'HTN_1', 'HTN_2', 'HTN_3', 'HTN_4', 'PTN',
      'MAP_max', 'MAP_min', 'range_change', 'range_overlap', 'range_size',
      'gain_mod', 'gain_mod_comm',
      'B1', 'B1_Comment', 'B2a', 'B2a_Comment', 'B2b', 'B2b_Comment', 'B3', 'B3_Comment',
      'C1', 'C1_Comment', 'C2ai', 'C2ai_Comment', 'C2aii', 'C2aii_Comment', 'C2bi',
      'C2bi_Comment', 'C2bii', 'C2bii_Comment', 'C2c', 'C2c_Comment', 'C2d',
      'C2d_Comment', 'C3', 'C3_Comment', 'C4a', 'C4a_Comment', 'C4b', 'C4b_Comment',
      'C4c', 'C4c_Comment', 'C4d', 'C4d_Comment', 'C4e', 'C4e_Comment', 'C4f',
      'C4f_Comment', 'C4g', 'C4g_Comment', 'C5a', 'C5a_Comment', 'C5b', 'C5b_Comment',
      'C5c', 'C5c_Comment', 'C6', 'C6_Comment', 'D1', 'D1_Comment', 'D2', 'D2_Comment',
      'D3', 'D3_Comment', 'D4', 'D4_Comment', 'GCM_or_Ensemble_name',
      'Historical_normal_period', 'Future_period', 'Emissions_scenario',
      'Link_to_source'
    )))

}

# read.csv("../../../Downloads/CCVI_data-2022-11-18 (1).csv") %>% colnames() %>% paste0(collapse = "', '")

update_restored <- function(df){
  # match column names to inputs and/or maybe reactive values?
  # will need some sort of lookup for what type of input needs to be updated
}
