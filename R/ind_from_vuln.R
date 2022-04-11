ind_from_vuln <- function(b_c_score, d_score, slr_vuln,
                          d_ie = FALSE, b_c_ie = FALSE, suff_dat_all = TRUE){
  if(!suff_dat_all){
    return("IE")
  }

  if(b_c_ie){
    b_c_index <- "IE"
  } else {

    # Convert score to index
    b_c_index <- as.character(cut(b_c_score, breaks = c(0, 4, 7, 10, Inf),
                                  labels = c("LV", "MV", "HV", "EV"),
                                  include.lowest = TRUE))
  }

  if(d_ie){
    d_index <- "IE"
  } else {
    d_index <- as.character(cut(d_score, breaks = c(0, 2, 4, 6, Inf),
                                  labels = c("LV", "MV", "HV", "EV"),
                                right = FALSE,
                                include.lowest = TRUE))
  }

  col_bc_index <- which(comb_index_tbl$Dindex == b_c_index) + 1
  row_d_index <- which(comb_index_tbl$Dindex == d_index)

  if(slr_vuln){
    "EV"
  } else {
    comb_index_tbl[row_d_index, col_bc_index]
  }
}
