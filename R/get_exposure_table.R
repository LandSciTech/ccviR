get_exposure_table <- function(spattbl, varname, clim_readme, brks){

  if(varname == "MAT"){
    exp_val <- "temp_exp_cave"
    class_col <- html("Temperature class")
    breaks_col <- html("Temperature change (&deg;C)")
  }

  if(varname == "CMD"){
    exp_val <- "moist_exp_cave"
    class_col <- html("Moisture class")
    breaks_col <- html("Moisture change (mm)")
  }

  scenario_cols <- spattbl$scenario_name
  class_brks <- brks %>% unique()

  if(length(class_brks) == 1){

    class_brks <- class_brks %>% stringr::str_split_1(";") %>% sort()

    # create df with scenarios, classes, and breaks
    scn_brks_df <- spattbl %>%
      select(scenario_name, matches(paste0(varname, "_\\d"))) %>%
      tidyr::pivot_longer(-scenario_name) %>%
      tidyr::pivot_wider(names_from = scenario_name, values_from = value) %>%
      mutate(class_brks = class_brks) %>%
      tidyr::separate(class_brks, into = c("class", "breaks"), sep = ": ") %>%
      mutate(breaks = stringr::str_replace_all(breaks, "[()]", "")) %>%
      mutate(breaks = stringr::str_replace_all(breaks, " - ", " to ")) %>%
      select(class, breaks, scenario_cols)

    # create df with exposure multipliers
    exp_df <- spattbl %>% select(scenario_name, contains("exp_cave")) %>%
      tidyr::pivot_longer(-scenario_name) %>%
      tidyr::pivot_wider(names_from = scenario_name, values_from = value) %>%
      mutate(breaks = "Exposure Multiplier")

    # add exposure multipliers to scenarios, classes, and breaks
    exp_res_df <- bind_rows(scn_brks_df, exp_df %>%
                               filter(name == exp_val) %>%
                               select(-name)) %>%
      mutate_if(is.numeric, round, digits = 2)

    # create table
    exp_res_tbl <- gt::gt(exp_res_df) %>%
      cols_label(class = class_col, breaks = breaks_col) %>%
      tab_spanner(label = "Proportion of range", columns = scenario_cols) %>%
      tab_options(table.width = 600,
                  table.font.size = 12,
                  column_labels.padding.horizontal = 10,
                  column_labels.padding = 2,
                  data_row.padding = 2) %>%
      cols_align(align = "center", columns = everything()) %>%
      sub_missing(missing_text = "") %>%
      tab_style(style = cell_text(weight = "bold", v_align = "middle"),
                location = cells_column_labels(columns = 1:2)) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                location = cells_column_spanners()) %>%
      tab_style_body(style = cell_text(weight = "bold"),
                     values = "Exposure Multiplier") %>%
      tab_style_body(style = list(cell_fill(color = "#F8F8F8"),
                                  cell_borders(side = c("top", "bottom"),
                                               weight = px(2), color = "lightgray")),
                     values = "Exposure Multiplier", targets = "row")
  } else {

    class_brks <- clim_readme %>%
      select(Scenario_Name, matches(varname)) %>%
      mutate(scenario_name = stringr::str_replace_all(Scenario_Name, "\t", "")) %>%
      select(scenario_name, matches(varname)) %>%
      tidyr::separate(matches(varname), into = c("6", "5", "4", "3", "2", "1"), sep = ";") %>%
      tidyr::pivot_longer(-scenario_name) %>%
      tidyr::separate(value, into = c("class", "breaks"), sep = ": ") %>%
      mutate(breaks = stringr::str_replace_all(breaks, "[()]", "")) %>%
      mutate(breaks = stringr::str_replace_all(breaks, " - ", " to ")) %>%
      select(scenario_name, class, breaks)


    # create df with scenarios, classes, and breaks
    scn_brks_df <- spattbl %>%
      select(scenario_name, matches(paste0(varname, "_\\d"))) %>%
      mutate(scenario_name = stringr::str_replace_all(scenario_name, "\t", "")) %>%
      tidyr::pivot_longer(-scenario_name) %>%
      mutate(class = stringr::str_replace(name, paste0(varname, "_"), "")) %>%
      merge(class_brks, by = c("scenario_name", "class")) %>%
      select(scenario_name, class, breaks, value)

    # create df with exposure multipliers
    exp_df <- spattbl %>% select(scenario_name, contains("exp_cave")) %>%
      mutate(scenario_name = stringr::str_replace_all(scenario_name, "\t", "")) %>%
      tidyr::pivot_longer(-scenario_name) %>%
      mutate(breaks = "Exposure Multiplier")

    # add exposure multipliers to scenarios, classes, and breaks
    exp_res_df <- bind_rows(scn_brks_df, exp_df %>%
                              filter(name == exp_val) %>%
                              select(-name)) %>%
      mutate_if(is.numeric, round, digits = 2) %>%
      group_by(scenario_name)

    # create table
    exp_res_tbl <- gt::gt(exp_res_df) %>%
      cols_label(class = class_col, breaks = breaks_col, value = "Proportion of range") %>%
      tab_options(table.width = 600,
                  table.font.size = 12,
                  column_labels.padding.horizontal = 10,
                  column_labels.padding = 2,
                  data_row.padding = 2) %>%
      cols_align(align = "center", columns = everything()) %>%
      sub_missing(missing_text = "") %>%
      tab_style(style = cell_text(weight = "bold", v_align = "middle"),
                location = cells_column_labels()) %>%
      tab_style(style = cell_text(weight = "bold"),
                location = cells_row_groups()) %>%
      tab_style_body(style = cell_text(weight = "bold"),
                values = "Exposure Multiplier") %>%
      tab_style_body(style = list(cell_fill(color = "#F8F8F8"),
                             cell_borders(side = c("top", "bottom"),
                                          weight = px(2), color = "lightgray")),
                values = "Exposure Multiplier", targets = "row")
  }

  exp_res_tbl

}


