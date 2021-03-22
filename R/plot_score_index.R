#' make a visualization of the D score, B/C score and index

plot_score_index <- function(b_c_score, d_score){
  score_pt <- data.frame(d_score = d_score, b_c_score = b_c_score)

  # max possible score
  max_score_bc <- 22*6.6 + 3

  max_score_d <- 11

  d_score_lim <- ifelse(d_score > 7, d_score + 1, 8)

  b_c_score_lim <- ifelse(b_c_score > 20, b_c_score + 5, 20)

  comb_index_tbl <- data.frame(Dindex = c("EV", "HV", "MV", "LV", "IE"),
                               bc_ev = c("EV", "EV", "HV", "HV", "EV"),
                               bc_hv = c("EV", "HV", "HV", "MV", "HV"),
                               bc_mv = c("HV", "HV", "MV", "MV", "MV"),
                               bc_lv = c("HV", "MV", "LV", "LV", "LV"),
                               stringsAsFactors = FALSE)

  levs <- rev(c("EV", "HV", "MV", "LV", "IE"))

  comb_index_tbl <- comb_index_tbl %>% tidyr::pivot_longer(-Dindex) %>%
    transmute(Dindex,
              Bindex = name %>% stringr::str_remove("bc_") %>% toupper(),
              value = factor(value, levels = levs))

  score_tbl <- expand.grid(b_c_score = seq(0,max_score_bc),
                           d_score = seq(0, max_score_d)) %>%
    mutate(d_index = case_when(d_score >= 6 ~ "EV",
                               d_score >= 4 ~ "HV",
                               d_score >= 2 ~ "MV",
                               TRUE ~ "LV"),
           b_c_index = case_when(b_c_score > 10 ~ "EV",
                                 b_c_score > 7 ~ "HV",
                                 b_c_score > 4 ~ "MV",
                                 TRUE ~ "LV")) %>%
    left_join(comb_index_tbl, by = c(d_index = "Dindex", b_c_index = "Bindex"))

  ggplot(score_tbl, aes(b_c_score, d_score, fill = value))+
    geom_tile(alpha = 0.6)+
    coord_cartesian(xlim = c(0, b_c_score_lim), ylim = c(0, d_score_lim))+
    scale_fill_viridis_d()+
    theme_classic()+
    geom_point(data = score_pt, aes(b_c_score, d_score),
               shape = 4, stroke = 3,
               inherit.aes = FALSE)+
    labs(x = "Sections B and C Score", y = "Section D Score", fill = "Index")
}

