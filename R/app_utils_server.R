#' Loads a previously saved data set from a shinyFiles list
#'
#' @param path Character. File path
#'
#' @returns Loaded data frame
#' @noRd
#'
#' @examples
#' load_previous(test_files()$saved$full_run)

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

parse_path <- function(volumes, shiny_files_list) {
  req(!is.integer(shiny_files_list))
  parseFilePaths(volumes, shiny_files_list)$datapath
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





prep_raster_map <- function(r, r_nm, max_cell, quiet = FALSE) {

  if(!is.null(r)){
    rast_ncell <- terra::ncell(r)
    with_progress(message = paste("Preparing Raster:", r_nm), {
      if(rast_ncell > max_cell) {
        fct <- ceiling(sqrt(rast_ncell/max_cell))
        r <- terra::aggregate(r, fact = fct, fun = "modal", na.rm = TRUE)
        inform_prog("Aggregating raster for faster plotting", quiet = quiet)
      }
      if(!terra::same.crs(r, "EPSG:3857")){
        # Problem with re-projecting some maps, NAs become numbers?
        # https://github.com/rspatial/terra/issues/1356
        # Only problem is a warning... (suppressed for now)
        #
        # A workaround seems to be converting to in memory
        # (but this may result in long computational times)
        #
        # Uncomment this section if required
        #inform_prog("Converting raster to in-memory for re-projection", quiet)
        #r <- terra::toMemory(r)
        inform_prog("Projecting raster")
        r <- terra::project(r, "EPSG:3857", method = "near")
      }

      names(r) <- stringr::str_replace_all(names(r), "_", " ")
    })
  }
  return(r)
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

widen_vuln_coms <- function(questions) {

  comments <- bind_elements(questions, "comments")
  evidence <- bind_elements(questions, "evidence")

  scn_n <- purrr::map_dbl(bind_elements(questions, "questions")$Value1, length) |>
    max()

  # Keep scenarios as separate rows
  vuln_df <- bind_elements(questions, "questions") %>%
    select("Code", matches("Value\\d")) %>%
    filter(!.data$Code %in% c("Z2", "Z3")) %>%
    arrange(.data$Code) %>%
    mutate(n = list(seq_len(scn_n))) %>%
    tidyr::unnest(c("Value1", "n")) %>%
    tidyr::unite("Value", "Value1":"Value4", na.rm = TRUE, sep = ", ") %>%
    left_join(comments, by = "Code") %>%
    left_join(evidence, by = "Code") %>%
    tidyr::pivot_wider(id_cols = "n",
                       names_from = "Code",
                       values_from = c("com", "evi", "Value")) %>%
    rename_all(~stringr::str_remove(.x, "Value_")) %>%
    select(-"n")

  select(vuln_df, order(colnames(vuln_df)))
}

combine_outdata <- function(species_data, questions, spat_run, spat_res, index) {

  # Priority order is species, question inputs, spatial run, spatial restored

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


# Update UI based on values loaded from csv
update_restored <- function(df, session, section = NULL){
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

  # If empty values, reset the inputs
  df_reset <- filter(df2, is.na(.data$value) | .data$value == "")
  purrr::walk(df_reset$input, shinyjs::reset)

  # run the appropriate update function for each input
  # tricky part is supplying the right argument name for the update fun
  df2 <- anti_join(df2, df_reset, by = "input")
  if(nrow(df2) > 0) purrr::pwalk(df2, update_call, session = session)
}

# build the call to update function from the inputs
update_call <- function(input, update_fun, value, arg_name, comment, evidence, session){

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

spat_vuln_hide <- function(id, ...) {
  ready <- is_ready(list(...)) && # Any errors in ...?
    all(vapply(list(...), isTruthy, logical(1))) # Any NULLs?
  shinyjs::toggle(paste0(id, "div"), condition = ready)
}

# TODO: Move to app_utils_ui.R? Isn't this a UI creator?
render_spat_vuln_box <- function(id, ui_id, spat_df, input, chk_label = NULL,
                                 multi_stop = FALSE, is_spatial = FALSE) {

  com_id <- paste0("com_", ui_id)
  evi_id <- paste0("evi_", ui_id)

  # get previous comment/evidence
  prevCom <- isolate(input[[com_id]])
  prevCom <- ifelse(is.null(prevCom), "", prevCom)
  prevEvi <- isolate(input[[evi_id]])
  prevEvi <- if(is.null(prevEvi)) "" else prevEvi # Permits lengths > 1

  # If have spatial analysis AND have data for this Question
  # is_spatial is only true if we definitely have a spatial contribution here
  # (i.e. not just previously answered questions in teh spat_df)
  if(is_spatial && isTruthy(spat_df) && all(!is.na(spat_df[[ui_id]]) & spat_df[[ui_id]] > -1)) {
    box_val <- spat_df[[ui_id]] %>% unique()
    if(is.character(box_val)) box_val <- stringr::str_split_1(box_val, ", ?")
    if(all(prevEvi == "")) prevEvi <- "Spatial Analysis - ccviR"
  } else {
    # If no current spatial values, use previous values (either manually entered, or missing)
    box_val <- input[[ui_id]]
  }

  check_comment_ui(id, ui_id, label = NULL, chk_label = chk_label,
                   choiceNames = valueNms,
                   choiceValues = valueOpts,
                   selected = box_val,
                   com = prevCom, evi = prevEvi,
                   spatial = TRUE, multi_stop = multi_stop)
}

# recreate index_res object on loading from csv
recreate_index_res <- function(df){
  if(is.null(df$CCVI_index) || all(is.na(df$CCVI_index))){
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

switch_tab <- function(tab, parent_session) {
  updateTabsetPanel(session = parent_session, inputId = "tabset", selected = tab)
  shinyjs::runjs("window.scrollTo(0, 0)")
}

track_mandatory <- function(m, input) {
  all_filled <- vapply(m,
                   function(x) !is.null(input[[x]]) && input[[x]] != "",
                   FUN.VALUE = logical(1)) %>%
    all()
  shinyjs::toggleState(id = "continue", condition = all_filled)
}

show_guidelines <- function(input) {
  # Show guidelines with additional info for each section
  help_ins <- stringr::str_subset(names(input), "help")

  purrr::map(help_ins,
             ~observeEvent(input[[.x]], {
               guide_popup(.x)
             }, ignoreInit = TRUE))
}

collect_questions <- function(input, section, tax_grp = NULL, spatial = NULL) {

  # Use a predefined list so we update as changes added
  # - if we use names(input) - invalidates constantly
  # - if we use names(isolate(input)) - doesn't update when dynamic UIs added

  qs <- vulnq_code_lu_tbl$Code %>%
    stringr::str_subset(paste0("^", section))

  type <- stringr::str_sub(qs[1], 1, 1)

  q <- purrr::map_df(qs, ~getMultValues(input[[.x]], .x)) %>%
    as_tibble()

  if(type == "C") { # Where taxa influences questions
    q <- filter_qs(q, tax_grp, c("Vascular Plant", "Nonvascular Plant"))
  }

  # Add in the spatial D questions which can only come spatial data
  # Only applies if multiple scenarios (i.e. rows > 1)
  if(type == "D" && !is.null(spatial) &&
     any(!is.null(spatial[, c("D2", "D3", "D4")]) & spatial[, c("D2", "D3", "D4")] != -1) &&
     nrow(spatial) > 1) {
    for(i in c("D2", "D3", "D4")) q$Value1[q$Code == i] <- list(spatial[[i]])
  }
  q$Value1 <- as.list(q$Value1)

  c <- purrr::map_df(paste0("com_", qs), ~{
    data.frame(Code = stringr::str_remove(.x, "com_"),
               com = if(!is.null(input[[.x]])) input[[.x]] else NA)
  })

  e <- purrr::map(paste0("evi_", qs), ~{
    dplyr::tibble(Code = stringr::str_remove(.x, "evi_"),
                  evi = if(!is.null(input[[.x]])) list(input[[.x]]) else NA)
  }) %>%
    purrr::list_rbind()

  list("questions" = q, "comments" = c, "evidence" = e)
}

bind_elements <- function(questions, type) {
  col <- stringr::str_extract(type, "evi|com")
  out <- questions %>%
    purrr::map(~.x()[[type]]) %>%
    purrr::list_rbind()

  if(!is.na(col)) {
    out <- dplyr::mutate(
      out, !!col := purrr::map_chr(.data[[col]], ~paste(sort(.x), collapse = ", ")))
  }
  out
}

answered_n <- function(questions) {

  type <- stringr::str_sub(questions$questions$Code[1], 1, 1)

  q <- questions$questions %>%
    dplyr::rowwise() %>%
    # Here score = answered/not answered (NOT ACTUAL SCORE)
    dplyr::mutate(score = any(dplyr::pick(-"Code") >= 0, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$Value1))

  # How many Q5's to omit from count?
  if(type == "C") {
    q$c5 <- sum(!is.na(q$score[q$Code %in% c("C5a", "C5b", "C5c")])) - 1
  } else q$c5 <- 0

  # Get evidence after having resolved questions
  e <- questions$evidence %>%
    dplyr::filter(.data$Code %in% q$Code[isTruthy(q$score)])

  dplyr::left_join(q, e, by = "Code") %>%
    dplyr::mutate(sec = .env$type) %>%
    dplyr::select("sec", "Code", "score", "evi", "c5")
}

count_n <- function(questions) {
  questions %>%
    dplyr::summarize(q_total = sum(!is.na(.data$score)) - .data$c5[1],
                     q_ans = sum(.data$score > 0, na.rm = TRUE),
                     q_txt = paste0(.data$q_ans, "/", .data$q_total),
                     e_total = .data$q_ans,
                     e_ans = sum(purrr::map_lgl(.data$evi, isTruthy)),
                     e_txt = paste0(.data$e_ans, "/", .data$e_total),
                     .by = "sec") %>%
    dplyr::left_join(dplyr::tibble(sec = c("B", "C", "D"),
                                   req = c( 3,  10,   1)),
                     by = "sec")
}

report_n <- function(questions) {

  q <- answered_n(questions) %>%
    count_n()

  q_txt <- paste0(q$q_txt, " questions answered")

  if(q$q_ans >= q$req) {
    q_txt <- tagList(icon("check", style = "color:green"), q_txt)
  } else {
    q_txt <- tagList(icon("xmark", style = "color:red"), q_txt,
                     strong("(Insufficient, ", HTML("&ge;", .noWS = "after"),
                            q$req, "required)"))
  }

  e_txt <- paste0(q$e_txt, " evidence supplied")

  if(q$e_ans == q$e_total) {
    e_txt <- tagList(icon("check", style = "color:green"), e_txt)
  } else {
    e_txt <- tagList(
      icon("xmark", style = "color:red"), e_txt,
      strong("(Evidence must be supplied for each Question answered)"))
  }

  tagList(q_txt, br(), e_txt)
}

# Compare Questions in the index to questions in the app to see if they
# are in synchrony.
index_match_qs <- function(questions_wide, index) {

  qs <- questions_wide %>%
    dplyr::select(-dplyr::contains("com_"), -dplyr::contains("evi_")) %>%
    # Omit removed questions (not the same as unanswered)
    dplyr::select(-dplyr::where(~all(.x == "") | all(.x == -1))) %>%
    distinct()

  index <- select(index, matches("^[BCD]{1}\\d{1}[a-z]{0,3}")) %>%
    dplyr::select(-dplyr::where(~all(.x == "") | all(.x == -1))) %>%
    distinct()

  # If they match TRUE else FALSE
  nrow(qs) == nrow(index) && ncol(qs) == ncol(index) && all(qs == index)
}




show_questions <- function(tax_grp) {

  tax_lg <- dplyr::case_when(
    tax_grp %in% c("Vascular Plant", "Nonvascular Plant") ~ "Plant",
    tax_grp == "Lichen" ~ "Lichen",
    .default = "Animal")

  if(tax_lg == "Plant"){
    shinyjs::show("plant_only")
    shinyjs::show("plant_only2")
    shinyjs::hide("animal_only")
  }

  if(tax_lg == "Animal"){
    shinyjs::show("animal_only")
    shinyjs::hide("plant_only")
    shinyjs::hide("plant_only2")
  }

  if(tax_lg == "Lichen"){
    shinyjs::hide("animal_only")
    shinyjs::hide("plant_only")
    shinyjs::hide("plant_only2")
  }
}


#' Check and load spatial vector data
#'
#' @noRd

read_poly <- function(pth, name, req = FALSE) {

  # Checks
  if(!req & !isTruthy(pth)) return(NULL) # If it can be NULL and is null, return NULL
  req(pth)
  validate(need(fs::file_exists(pth), "File does not exist"))

  # Read file
  notify(paste("Loading", name))
  s <- try(sf::st_read(pth, agr = "constant", quiet = TRUE), silent = TRUE)
  validate(need(
    !inherits(s, "try-error"),
    "Error reading file. Are you sure this is a valid polygon spatial file?"))

  check_polys(s, name)

}

#' Check and load spatial raster data
#'
#' @noRd
read_raster <- function(pth, name, scn_nms = NULL, req = FALSE) {

  # Checks

  # If it can be NULL and is null, return NULL
  if(!req & all(vapply(pth, is.null, TRUE))) return(NULL)

  if(is.list(pth)) {
    pth <- unlist(pth)
    pth <- pth[sort(names(pth))]
  }

  names(pth) <- fs::path_file(pth) %>% fs::path_ext_remove()

  req(pth)

  if(length(pth) > 1) {
    req(scn_nms)
    if(length(pth) != length(scn_nms)) {
      stop("Unexpected mismatch between rng_chg inputs and scenario names",
           call. = FALSE)
    }
  }

  # Read file
  notify(paste("Loading", name))

  r <- try({
    t <- pth %>%
      terra::rast() %>%
      check_trim()
    if(!is.null(scn_nms) && length(scn_nms) == length(pth)) {
      terra::set.names(t, scn_nms)
    }
    t
  }, silent = TRUE)

  n <- length(scn_nms)
  validate(need(
    !inherits(r, "try-error"),
    paste("Cannot open ",
          if(n == 1) "this" else "these",
          if(n == 1) "file" else "files",
          "as SpatRaster")
  ))

  check_rast(r, name)

  r
}

read_clim <- function(pth, scn_nms) {
  notify("Loading climate data rasters")
  get_clim_vars(pth, scenario_names = scn_nms)
}

read_clim_readme <- function(pth) {
  notify("Loading climate data readme")
  pth <- fs::path(pth, "climate_data_readme.csv")
  req(fs::file_exists(pth))
  utils::read.csv(pth, check.names = FALSE)
}

notify <- function(msg) {

  id <- showNotification(msg, duration = NULL, closeButton = FALSE)
  # Use `eval()` to apply to exiting the parent function
  eval(on.exit(removeNotification(id), add = TRUE), envir = parent.frame())
  if(isTruthy(getOption("ccviR.debug"))) message(msg)
}




is_ready <- function(reactive) {
  tryCatch({
    reactive
    TRUE
  }, error = function(cond) FALSE)
}
