#' Visualize the D score, B/C score and index
#'
#' A graph to visualize where the result falls relative to the thresholds used
#' to calculate index values and how the combination of the D score and B/C
#' score affects the index value.
#'
#' @param b_c_score Total score in the B and C sections.
#' @param d_score Total score in the D section
#' @param n_d_factors Number of factors scored in the D section
#'
#' @return ggplot2 graph.
#'
#' @export
#' @examples
#' base_pth <- system.file("extData", package = "ccviR")
#'
#' clim_vars <- get_clim_vars(file.path(base_pth, "clim_files/processed"))
#'
#' spat_res <- run_spatial(
#'   range_poly = sf::read_sf(file.path(base_pth, "rng_poly_high.shp")),
#'   scale_poly = sf::read_sf(file.path(base_pth, "assess_poly.shp")),
#'   clim_vars_lst = clim_vars,
#'   hs_rast = raster::raster(file.path(base_pth, "HS_rast_high.tif")),
#'   hs_rcl = matrix(c(0:7, 0, 1, 2, 2 ,2, 2, 2, 3), ncol = 2)
#' )
#'
#' # vulnerability factor table with score 1 (somewhat increase vulnerability)
#' # for all factors
#' vuln <- make_vuln_df("test_species", val1 = 1, mig = 1)
#'
#' index_vuln <- calc_vulnerability(spat_res$spat_table, vuln, "Bird")
#'
#' plot_score_index(index_vuln$b_c_score, index_vuln$d_score, index_vuln$n_d_factors)
plot_score_index <- function(score_df){
  # if b_c is IE no plot if d is IE set to 0 but still plot
  if(all(score_df$n_b_factors < 3)||all(score_df$n_c_factors < 10)){
    return(NULL)
  }

  score_pt <- score_df %>%
    mutate(d_score = ifelse(n_d_factors == 0, -1, d_score))


  # max possible score
  max_score_bc <- 22*6.6 + 3

  max_score_d <- 11

  score_lim <- mutate(score_pt,
                     d_score_lim = ifelse(d_score > 7, d_score + 1, 8),
                     b_c_score_lim = ifelse(b_c_score > 20, b_c_score + 5, 20)) %>%
    summarise(across(contains("lim"), .fns = max))


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


  good_shapes <- c(4, 3, 8, 15, 16, 17)
  if(nrow(score_pt) > 6){
    shp_vals <- LETTERS[1:nrow(score_pt)]

  } else {
    shp_vals <- good_shapes[1:nrow(score_pt)]
  }

  ggplot2::ggplot(score_tbl, ggplot2::aes(b_c_score, d_score, fill = value))+
    ggplot2::geom_raster(alpha = 0.6, hjust = 0, vjust = 0.5)+
    ggplot2::coord_fixed(xlim = c(0, score_lim$b_c_score_lim),
                         ylim = c(-1.5, score_lim$d_score_lim))+
    ggplot2::scale_fill_manual(values = c("#008000", "#FFC125", "#FF8C00", "#FF0000"))+
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(),
                                breaks = c(-1:score_lim$d_score_lim))+
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(),
                                breaks = scales::breaks_extended(10))+
    ggplot2::theme_classic()+
    ggplot2::geom_point(data = score_pt,
                        ggplot2::aes(b_c_score, d_score, shape = scenario_name),
                        stroke = 2,
                        size = 2,
                        inherit.aes = FALSE)+
    ggplot2::scale_shape_manual(values = shp_vals)+
    ggplot2::labs(x = "Exposure x Sensitivity and Adaptive Capacity",
                  y = "Modelled Response to Climate Change", fill = "Index",
                  shape = "Scenario")
}

