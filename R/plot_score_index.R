#' make a visualization of the D score, B/C score and index
#'
#' @export
plot_score_index <- function(b_c_score, d_score, n_d_factors){
  score_pt <- data.frame(d_score = ifelse(n_d_factors == 0, -1, d_score),
                         b_c_score = b_c_score)

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
                           d_score = seq(-1, max_score_d)) %>%
    mutate(d_index = case_when(d_score >= 6 ~ "EV",
                               d_score >= 4 ~ "HV",
                               d_score >= 2 ~ "MV",
                               d_score < 0 ~ "IE",
                               TRUE ~ "LV"),
           b_c_index = case_when(b_c_score > 10 ~ "EV",
                                 b_c_score > 7 ~ "HV",
                                 b_c_score > 4 ~ "MV",
                                 TRUE ~ "LV")) %>%
    left_join(comb_index_tbl, by = c(d_index = "Dindex", b_c_index = "Bindex"))

  ggplot2::ggplot(score_tbl, ggplot2::aes(b_c_score, d_score, fill = value))+
    ggplot2::geom_raster(alpha = 0.6, hjust = 0, vjust = 0.5)+
    ggplot2::coord_fixed(xlim = c(0, b_c_score_lim), ylim = c(-1.5, d_score_lim))+
    ggplot2::scale_fill_manual(values = c("#008000", "#FFC125", "#FF8C00", "#FF0000"))+
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(),
                                breaks = c(-1:d_score_lim))+
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(),
                                breaks = scales::breaks_extended(10))+
    ggplot2::theme_classic()+
    ggplot2::geom_point(data = score_pt, ggplot2::aes(b_c_score, d_score),
               shape = 4, stroke = 3,
               inherit.aes = FALSE)+
    ggplot2::labs(x = "Exposure x Sensitivity and Adaptive Capacity",
                  y = "Modelled Response to Climate Change", fill = "Index")
}

