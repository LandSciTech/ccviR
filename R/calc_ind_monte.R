
#' Monte Carlo Index Calculation
#'
#' Calculate the index by randomly choosing one of the selected answers
#'
#'
#' @noRd
#'
#'

calc_ind_monte <- function(vuln_df, n_rnds, d_ie){
  vuln_df <- filter(vuln_df, !is.na(.data$score), .data$score >= 0) %>%
    mutate(n_boxes = num_not_na(.data$Value1) + num_not_na(.data$Value2) +
             num_not_na(.data$Value3) + num_not_na(.data$Value4),
           thold = 1/.data$n_boxes)

  n_facts <- nrow(vuln_df)

  vuln_df <- vuln_df[rep(seq_len(n_facts), n_rnds), ]

  vuln_df <- vuln_df %>%
    mutate(round_id = rep(seq_len(n_rnds), n_facts) %>% sort(),
           rnd_num = runif(n()),
           which_val = ifelse(.data$rnd_num < .data$thold, 1,
                              ifelse(.data$rnd_num < .data$thold *2, 2,
                                     ifelse(.data$rnd_num < .data$thold*3, 3, 4)))) %>%
    select(.data$round_id, .data$Code, matches("Value\\d"), .data$exp, .data$which_val) %>%
    tidyr::pivot_longer(matches("Value\\d"), names_to = "box", values_to = "value") %>%
    mutate(box = stringr::str_extract(.data$box, "\\d") %>% as.numeric()) %>%
    filter(.data$box == .data$which_val) %>%
    mutate(score = .data$value * .data$exp)

  vuln_sum_df <- vuln_df %>% group_by(.data$round_id) %>%
    summarise(b_c_score = sum(ifelse(stringr::str_detect(.data$Code, "[B,C]\\d.*"),
                                     .data$score, NA), na.rm = TRUE),
              d_score = sum(ifelse(stringr::str_detect(.data$Code, "[D]\\d.*"),
                                   .data$score, NA), na.rm = TRUE),
              slr_vuln = all(sum(ifelse(.data$Code == "B1", .data$value, NA),
                                 na.rm = TRUE) == 3,
                             sum(ifelse(.data$Code %in% c("B2a", "B2b"), .data$value, NA),
                                 na.rm = TRUE) >= 2,
                             sum(ifelse(.data$Code == "C1", .data$value, NA),
                                 na.rm = TRUE) >= 2)) %>%
    mutate(index = ind_from_vuln(.data$b_c_score, .data$d_score, .data$slr_vuln, d_ie = d_ie))

}

num_not_na <- function(x){
  as.numeric(!is.na(x))
}


