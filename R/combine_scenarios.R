# vuln_calc_lst <- res

combine_scenarios <- function(vuln_calc_lst){
  b_c_scores <- purrr::map_dbl(vuln_calc_lst, "b_c_score")
}

