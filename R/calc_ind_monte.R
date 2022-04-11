
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
    mutate(Value1 = sample.vec(na.omit(c(Value1, Value2, Value3, Value4)), size = 1)) %>%
    select(-c(Value2, Value3, Value4)) %>%
    ungroup() %>%
    mutate(score = ifelse(Value1 < 0, 0, exp * Value1))

  b_c_score <- vuln_df %>%
    filter(stringr::str_detect(Code, "[B,C]\\d.*"), score >= 0) %>%
    pull(score) %>% sum(na.rm = TRUE)

  d_score <- vuln_df %>%
    filter(stringr::str_detect(Code, "[D]\\d.*"), score >= 0) %>%
    pull(score) %>% sum(na.rm = TRUE)

  # sea level rise is greatly increase, either anthro or nat barriers are
  # increase or greatly increase and dispersal is increase or greatly increase
  slr_vuln <- all(vuln_df %>% filter(Code == "B1") %>%
                    select(matches("Value\\d")) %>%
                    max(na.rm = TRUE) == 3,
                  vuln_df %>% filter(Code %in% c("B2a", "B2b")) %>%
                    select(matches("Value\\d")) %>%
                    max(na.rm = TRUE) >= 2,
                  vuln_df %>% filter(Code == "C1") %>%
                    select(matches("Value\\d")) %>%
                    max(na.rm = TRUE) >= 2)

  ind_from_vuln(b_c_score, d_score, slr_vuln)
}


