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

  # TODO: Replace with validate(need())
  validate(need(fs::is_file(path) & fs::file_exists(path), "File doesn't exist"))

  df <- tryCatch(error = function(cnd) {
    validate(need(TRUE, "CSV file is empty, cannot restore from file."))
  },
  read.csv(path)
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
    mutate(update_fun = if_else(stringr::str_detect(update_fun, "_"),
                                paste0(update_fun, "2"), update_fun)) %>%
    mutate(arg_name = intersect( c("selected", "value"), formalArgs(.data$update_fun)))

  # this is used as a trigger to skip running spatial until after returning to
  # UI so that input is updated with values from csv
  updateTextInput(session, inputId = "hidden", value = "yes")

  # Catch both "spatial" and "spatial_range_change" in "spatial" .env$section
  df2 <- filter(df2, stringr::str_detect(.data$section, .env$section))
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
      update_fun(session = session, inputId = input, value = value, com = comment, evi = evidence)
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



spat_vuln_hide2 <- function(id, spatial, values) {
  mis <- paste0("missing_", id)
  mapid <- paste0("map_", id)
  nmis <- paste0("not_missing_", id)
  tblid <- paste0("tbl_", id)

  # TODO: Replace this with validate(need())?

  # Do we have the spatial map?
  if(isTruthy(spatial)) {
    # Show everything (hide missing message)
    shinyjs::hide(mis)
    shinyjs::show(mapid)
    shinyjs::show(tblid)
    shinyjs::show(nmis)

  # Do we have the values?
  } else if(isTruthy(values)) {
    # Show table and message that not missing, but hide map (because we haven't recalculated the spatial data)
    shinyjs::hide(mis)
    shinyjs::hide(mapid)
    shinyjs::show(nmis)
    shinyjs::show(tblid)

    # Otherwise...
  } else {
    # Hide all details and show "missing" message
    shinyjs::show(mis)
    shinyjs::hide(mapid)
    shinyjs::hide(tblid)
    shinyjs::hide(nmis)
  }
}

render_spat_vuln_box2 <- function(id, ui_id, spat_df, input) {
  com_id <- NS(id, paste0("com", ui_id))
  evi_id <- NS(id, paste0("evi", ui_id))

  # get previous comment/evidence
  prevCom <- isolate(input[[com_id]])
  prevCom <- ifelse(is.null(prevCom), "", prevCom)
  prevEvi <- isolate(input[[evi_id]])
  prevEvi <- ifelse(is.null(prevEvi), "", prevEvi)

  if(isTruthy(spat_df)){
    box_val <- spat_df[[ui_id]] %>% unique()
    if(prevEvi == "") prevEvi <- "Spatial Analysis - ccviR"
  } else {
    box_val <- NULL
  }

  check_comment_ui2(id, ui_id, HTML("Calculated effect on vulnerability."),
                    choiceNames = valueNms,
                    choiceValues = valueOpts,
                    selected = box_val,
                    com = prevCom, evi = prevEvi)
}


widen_vuln_coms2 <- function(questions) {

  comments <- bind_elements(questions, "comments")
  evidence <- bind_elements(questions, "evidence")

  vuln_df <- bind_elements(questions, "questions") %>%
    select("Code", matches("Value\\d")) %>%
    filter(!.data$Code %in% c("Z2", "Z3")) %>%
    arrange(.data$Code) %>%
    mutate_all(as.character) %>%
    tidyr::unite("Value", .data$Value1:.data$Value4, na.rm = TRUE, sep = ", ") %>%
    left_join(comments, by = "Code") %>%
    left_join(evidence, by = "Code") %>%
    tidyr::pivot_wider(names_from = "Code",
                       values_from = c("com", "evi", "Value")) %>%
    rename_all(~stringr::str_remove(.x, "Value_"))

  select(vuln_df, order(colnames(vuln_df)))
}



combine_outdata2 <- function(out_data_lst){
  if(!is.null(out_data_lst$index)){
    out_data_lst$start <- out_data_lst$start %>%
      select(-any_of(colnames(out_data_lst$index)))
    out_data_lst$spat <- out_data_lst$spat %>%
      select(-any_of(colnames(out_data_lst$index)), -any_of(colnames(out_data_lst$start)))
  }

  exp_cols <- utils::read.csv(system.file("extdata/column_definitions_results.csv",
                                          package = "ccviR"))
  exp_nms <- exp_cols %>% filter(.data$Column.Name != "") %>%
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
    tidyr::separate_rows(.data$names_exp, sep = ",") %>%
    pull(.data$names_exp)

  out_dat <- bind_cols(out_data_lst) %>%
    select(any_of(exp_nms), contains("rng_chg_pth"))

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
