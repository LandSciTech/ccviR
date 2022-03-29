
#' Monte Carlo Index Calculation
#'
#' Calculate the index by randomly choosing one of the selected answers
#'
#'
#' @noRd
#'
#'

# sample samples 1:x if x has length 1
sample.vec <- function(x, ...) {
  if(length(x) == 1) {
    return(x)
  } else {
    sample(x, ...)
  }
}

calc_ind_monte <- function(vuln_df){
  vuln_df <- vuln_df %>%
    rowwise() %>%
    mutate(val = sample.vec(na.omit(c(Value1, Value2, Value3, Value4)), size = 1)) %>%
    ungroup() %>%
    mutate(score = ifelse(val < 0, 0, exp * val))


  b_c_score1 <- vuln_df %>% filter(stringr::str_detect(Code, "[B,C]\\d.*")) %>%
    pull(score) %>% sum(na.rm = TRUE)

  d_score1 <- vuln_df %>% filter(stringr::str_detect(Code, "[D]\\d.*")) %>%
    pull(score) %>% sum(na.rm = TRUE)

  # Convert score to index
  b_c_index <- case_when(b_c_score1 > 10 ~ "EV",
                         b_c_score1 > 7 ~ "HV",
                         b_c_score1 > 4 ~ "MV",
                         TRUE ~ "LV")

  d_index <- case_when(d_score1 >= 6 ~ "EV",
                       d_score1 >= 4 ~ "HV",
                       d_score1 >= 2 ~ "MV",
                       TRUE ~ "LV")

  # sea level rise is greatly increase, either anthro or nat barriers are
  # increase or greatly increase and dispersal is increase or greatly increase
  slr_vuln <- all(vuln_df %>% filter(Code == "B1") %>%
                    select(val) %>%
                    max(na.rm = TRUE) == 3,
                  vuln_df %>% filter(Code %in% c("B2a", "B2b")) %>%
                    select(val) %>%
                    max(na.rm = TRUE) >= 2,
                  vuln_df %>% filter(Code == "C1") %>%
                    select(val) %>%
                    max(na.rm = TRUE) >= 2)

  col_bc_index <- which(comb_index_tbl$Dindex == b_c_index) + 1
  row_d_index <- which(comb_index_tbl$Dindex == d_index)

  comb_index <- case_when( slr_vuln ~ "EV",
                           TRUE ~ comb_index_tbl[row_d_index, col_bc_index])
  return(comb_index)

}

