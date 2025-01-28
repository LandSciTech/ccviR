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
