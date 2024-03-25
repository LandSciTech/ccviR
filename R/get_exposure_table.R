get_exposure_table <- function(spattbl, varname, brks){

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

  class_brks <- brks %>% unique() %>% stringr::str_split_1(";") %>% sort()
  scenario_cols <- spattbl$scenario_name

  # create df with scenarios, classes, and breaks
  scn_brks_df <- spattbl %>% select(scenario_name, contains(varname)) %>%
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
    tab_style(style = cell_text(weight = "bold"),
              location = cells_body(columns = 2, rows = 7)) %>%
    tab_style(style = list(cell_fill(color = "lightgrey"),
                           cell_borders(side = "top", style = "hidden")),
              location = cells_body(rows = 7))

  exp_res_tbl

}


