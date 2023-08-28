apply_spat_tholds <-function(spat_df, cave){
  spat_df %>% rowwise() %>%
    mutate(
      temp_exp = case_when(
        .data$MAT_6 > 50 ~ 2.4,
        sum(.data$MAT_6, .data$MAT_5, na.rm = TRUE) >= 75 ~ 2,
        sum(.data$MAT_6, .data$MAT_5, .data$MAT_4, na.rm = TRUE) >= 60 ~ 1.6,
        sum(.data$MAT_6, .data$MAT_5, .data$MAT_4, .data$MAT_3, na.rm = TRUE) >= 40 ~ 1.2,
        sum(.data$MAT_6, .data$MAT_5, .data$MAT_4, .data$MAT_3, .data$MAT_2, na.rm = TRUE) >= 20 ~ 0.8,
        TRUE ~ 0.4
      ),
      temp_exp_cave = .data$temp_exp / ifelse(cave == 1, 3, 1),
      moist_exp = case_when(
        .data$CMD_6 >= 80 ~ 2,
        sum(.data$CMD_6, .data$CMD_5, na.rm = TRUE) >= 64 ~ 1.67,
        sum(.data$CMD_6, .data$CMD_5, .data$CMD_4, na.rm = TRUE) >= 48 ~ 1.33,
        sum(.data$CMD_6, .data$CMD_5, .data$CMD_4, .data$CMD_3, na.rm = TRUE) >= 32 ~ 1,
        sum(.data$CMD_6, .data$CMD_5, .data$CMD_4, .data$CMD_3, .data$CMD_2, na.rm = TRUE) >= 16 ~ 0.67,
        TRUE ~ 0.33
      ),
      moist_exp_cave = .data$moist_exp / ifelse(cave == 1, 3, 1),
      comb_exp = mean(c(.data$temp_exp, .data$moist_exp)),
      comb_exp_cave = .data$comb_exp / ifelse(cave == 1, 3, 1),
      C2ai = case_when(.data$HTN_1 > 10 ~ 0,
                       .data$HTN_2 > 10 ~ 1,
                       .data$HTN_3 > 10 ~ 2,
                       .data$HTN_4 > 10 ~ 3,
                       is.na(.data$HTN_1) ~ NA_real_),
      C2aii = case_when(.data$PTN > 90 ~ 3,
                        .data$PTN > 50 ~ 2,
                        .data$PTN > 10 ~ 1,
                        is.na(.data$PTN) ~ NA_real_,
                        TRUE ~ 0),
      range_MAP = .data$MAP_max - .data$MAP_min,
      C2bi = case_when(.data$range_MAP < 100 ~ 3,
                       .data$range_MAP < 254 ~ 2,
                       .data$range_MAP < 508 ~ 1,
                       is.na(.data$range_MAP) ~ NA_real_,
                       TRUE ~ 0),
      D2 = case_when(.data$range_change > 99 ~ 3,
                     .data$range_change > 50 ~ 2,
                     .data$range_change > 20 ~ 1,
                     is.na(.data$range_change) ~ -1,
                     TRUE ~ 0),
      D3 = case_when(.data$D2 == 3 ~ 0,
                     .data$range_overlap == 0 ~ 3,
                     .data$range_overlap < 30 ~ 2,
                     .data$range_overlap < 60 ~ 1,
                     is.na(.data$range_overlap) ~ -1,
                     TRUE ~ 0))
  # Code the vulnerability section as 3 = Greatly increase, 2 = Increase, 1 =
  # Somewhat increase, 0 = Neutral and -1 = Unknown
}
