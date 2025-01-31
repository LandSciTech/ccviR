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
    select(matches("^com_")) %>%
    tidyr::pivot_longer(everything(), names_to = "input",
                        names_prefix = "com_",
                        values_to = "comment",
                        values_transform = as.character) %>%
    mutate(comment = ifelse(is.na(comment), "", comment)) %>%
    distinct()

  # Catch input values
  df2 <- df %>%
    select(-matches("^com_")) %>%
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
    mutate(arg_name = intersect( c("selected", "value"), formalArgs(.data$update_fun)))

  # this is used as a trigger to skip running spatial until after returning to
  # UI so that input is updated with values from csv
  updateTextInput(session, inputId = "hidden", value = "yes")

  # Catch both "spatilal" and "spatial_range_change" in "spatial" .env$section
  df2 <- filter(df2, stringr::str_detect(.data$section, .env$section))
  df2 <- select(df2, -"section")

  # run the appropriate update function for each input
  # tricky part is supplying the right argument name for the update fun

  purrr::pwalk(df2, update_call, session = session)
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
  # get previous comment
  prevCom <- isolate(input[[com_id]])
  prevCom <- ifelse(is.null(prevCom), "", prevCom)

  if(isTruthy(spat_df)){
    box_val <- spat_df[[ui_id]] %>% unique()
  } else {
    box_val <- NULL
  }

  check_comment_ui2(id, ui_id, HTML("Calculated effect on vulnerability."),
                    choiceNames = valueNms,
                    choiceValues = valueOpts,
                    selected = box_val,
                    com = prevCom)
}


widen_vuln_coms2 <- function(questions) {

  comments <- bind_elements(questions, "comments")

  vuln_df <- bind_elements(questions, "questions") %>%
    select("Code", matches("Value\\d")) %>%
    filter(!.data$Code %in% c("Z2", "Z3")) %>%
    arrange(.data$Code) %>%
    mutate_all(as.character) %>%
    tidyr::unite("Value", .data$Value1:.data$Value4, na.rm = TRUE, sep = ", ") %>%
    left_join(comments, by = "Code") %>%
    tidyr::pivot_wider(names_from = "Code",
                       values_from = c("com","Value")) %>%
    rename_all(~stringr::str_remove(.x, "Value_"))


  select(vuln_df, order(colnames(vuln_df)))
}

bind_elements <- function(questions, type) {
  questions %>%
    purrr::map(~.x()[[type]]) %>%
    purrr::list_rbind()
}
