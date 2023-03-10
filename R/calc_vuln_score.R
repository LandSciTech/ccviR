calc_vuln_score <- function(vuln_df, spat_df){
  vuln_df <- vuln_df %>%
    mutate(exp = case_when(.data$Code %in% c("Z1", "Z2", "Z3") ~ NA_real_,
                           .data$Code %in% c("B1", "D1", "D2", "D3", "D4") ~ 1,
                           .data$Code %in% c("C2ai", "C2aii")~ spat_df$temp_exp_cave,
                           .data$Code %in% c("C2bi", "C2bii")~ spat_df$moist_exp_cave,
                           TRUE ~ spat_df$comb_exp_cave)) %>%
    group_by(.data$Code) %>%
    mutate(score = exp*mean(c(.data$Value1, .data$Value2, .data$Value3, .data$Value4),
                            na.rm = TRUE)) %>%
    ungroup()
}
