
#' Monte Carlo Index Calculation
#'
#' Calculate the index by randomly choosing one of the selected answers
#'
#'
#' @noRd
#'
#'

calc_ind_monte <- function(vuln_df, n_rnds){
  vuln_df <- filter(vuln_df, !is.na(score), score >= 0) %>%
    mutate(n_boxes = num_not_na(Value1) + num_not_na(Value2) +
             num_not_na(Value3) + num_not_na(Value4),
           thold = 1/n_boxes)

  n_facts <- nrow(vuln_df)

  vuln_df <- vuln_df[rep(seq_len(n_facts), n_rnds), ]

  vuln_df <- vuln_df %>%
    mutate(round_id = rep(seq_len(n_rnds), n_facts) %>% sort(),
           rnd_num = runif(n()),
           which_val = ifelse(rnd_num < thold, 1,
                              ifelse(rnd_num < thold *2, 2,
                                     ifelse(rnd_num < thold*3, 3, 4)))) %>%
    select(round_id, Code, matches("Value\\d"), exp, which_val) %>%
    tidyr::pivot_longer(matches("Value\\d"), names_to = "box", values_to = "value") %>%
    mutate(box = stringr::str_extract(box, "\\d") %>% as.numeric()) %>%
    filter(box == which_val) %>%
    mutate(score = value * exp)

  vuln_sum_df <- vuln_df %>% group_by(round_id) %>%
    summarise(b_c_score = sum(ifelse(stringr::str_detect(Code, "[B,C]\\d.*"),
                                     score, NA), na.rm = TRUE),
              d_score = sum(ifelse(stringr::str_detect(Code, "[D]\\d.*"),
                                   score, NA), na.rm = TRUE),
              slr_vuln = all(sum(ifelse(Code == "B1", value, NA),
                                 na.rm = TRUE) == 3,
                             sum(ifelse(Code %in% c("B2a", "B2b"), value, NA),
                                 na.rm = TRUE) >= 2,
                             sum(ifelse(Code == "C1", value, NA),
                                 na.rm = TRUE) >= 2)) %>%
    mutate(index = ind_from_vuln(b_c_score, d_score, slr_vuln))

}

num_not_na <- function(x){
  as.numeric(!is.na(x))
}


