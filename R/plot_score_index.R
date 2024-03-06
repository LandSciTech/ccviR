#' Visualize the D score, B/C score and index
#'
#' A graph to visualize where the result falls relative to the thresholds used
#' to calculate index values and how the combination of the D score and B/C
#' score affects the overall index value.
#'
#' The colours show the location of the thresholds used to determine the index,
#' the points show the score for each scenario and the lines show the range of
#' scores produced by the Monte Carlo simulations. Multiple scenarios are
#' identified by different symbols
#'
#' @param score_df A dataframe containing the result of a call to
#'   \code{calc_vulnerability}.
#'
#' @return ggplot2 graph.
#'
#' @export
#' @examples
#' base_pth <- system.file("extdata", package = "ccviR")
#'
#' # scenario names
#' scn_nms <- c("RCP 4.5", "RCP 8.5")
#'
#' clim_vars <- get_clim_vars(file.path(base_pth, "clim_files/processed"),
#'                            scenario_names = scn_nms)
#'
#' spat_res <- analyze_spatial(
#'   range_poly = sf::read_sf(file.path(base_pth, "rng_poly.shp"), agr = "constant"),
#'   scale_poly = sf::read_sf(file.path(base_pth, "assess_poly.shp"), agr = "constant"),
#'   clim_vars_lst = clim_vars,
#'   hs_rast = terra::rast(c(file.path(base_pth, "rng_chg_45.tif"),
#'                           file.path(base_pth, "rng_chg_85.tif"))),
#'   hs_rcl = matrix(c(-1, 0, 1, 1, 2, 3), ncol = 2),
#'   scenario_names = scn_nms
#' )
#'
#' # vulnerability factor table with score 1 (somewhat increase vulnerability)
#' # for all factors
#' vuln <- make_vuln_df("test_species", val1 = 1, mig = 1)
#' vuln$Value2[c(5, 7, 9)] <- 3
#'
#' index_vuln <- calc_vulnerability(spat_res$spat_table, vuln, "Bird")
#'
#' plot_score_index(index_vuln)
#'
plot_score_index <- function(score_df){
  # if b_c is IE no plot if d is IE set to 0 but still plot
  if(all(score_df$n_b_factors < 3)||all(score_df$n_c_factors < 10)){
    return(NULL)
  }

  score_pt <- score_df %>%
    mutate(d_score = ifelse(.data$n_d_factors == 0, -1, .data$d_score))

  score_pt <- score_pt %>%
    mutate(mc_results = purrr::map(.data$mc_results,
                                   ~summarise(.x, across(contains("score"),
                                                         lst(max, min))))) %>%
    tidyr::unnest("mc_results")

  # max possible score
  max_score_bc <- 22*6.6 + 3

  max_score_d <- 11

  score_lim <- mutate(
    score_pt,
    d_score_max = ifelse(is.na(.data$d_score_max), max(.data$d_score), .data$d_score_max),
    b_c_score_max = ifelse(is.na(.data$b_c_score_max), max(.data$b_c_score), .data$b_c_score_max),
    d_score_lim = ifelse(.data$d_score_max > 7, .data$d_score_max + 1, 8),
    b_c_score_lim = ifelse(.data$b_c_score_max > 18, .data$b_c_score_max + 5, 20)
  ) %>%
    summarise(across(contains("lim"), .fns = max))

  score_tbl <- expand.grid(b_c_score = seq(0, max_score_bc),
                           d_score = seq(-2, max_score_d)) %>%
    mutate(index = ind_from_vuln(.data$b_c_score, .data$d_score, slr_vuln = FALSE) %>%
             factor(levels = levels(comb_index_tbl$value)))


  good_shapes <- c(4, 3, 8, 15, 16, 17)
  if(nrow(score_pt) > 6){
    shp_vals <- LETTERS[1:nrow(score_pt)]

  } else {
    shp_vals <- good_shapes[1:nrow(score_pt)]
  }

  ggplot2::ggplot(score_tbl, ggplot2::aes(.data$b_c_score, .data$d_score, fill = .data$index))+
    ggplot2::geom_raster(alpha = 0.6, hjust = 0, vjust = 0.9)+
    ggplot2::coord_cartesian(xlim = c(-0.5, score_lim$b_c_score_lim),
                         ylim = c(-1.25, score_lim$d_score_lim), clip = "on")+
    ggplot2::scale_fill_manual(values = rev(c("#008000", "#FFC125", "#FF8C00", "#FF0000")))+
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(),
                                breaks = c(-1:score_lim$d_score_lim))+
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(),
                                breaks = scales::breaks_extended(10))+
    ggplot2::geom_linerange(data = score_pt,
                            ggplot2::aes(.data$b_c_score, .data$d_score, ymin = .data$d_score_min,
                                         ymax = .data$d_score_max),
                            col = "black",
                            linewidth = 1.25, alpha = 0.4,
                            inherit.aes = FALSE, na.rm = TRUE)+
    ggplot2::geom_linerange(data = score_pt,
                           ggplot2::aes(y = .data$d_score, xmin = .data$b_c_score_min,
                                        xmax = .data$b_c_score_max),
                           col = "black",
                           linewidth = 1.25, alpha = 0.4,
                           inherit.aes = FALSE, na.rm = TRUE)+
    ggplot2::geom_point(data = score_pt,
                        ggplot2::aes(.data$b_c_score, .data$d_score, shape = .data$scenario_name),
                        stroke = 2,
                        size = 2,
                        inherit.aes = FALSE)+
    ggplot2::scale_shape_manual(values = shp_vals)+
    ggplot2::labs(x = "Exposure x Sensitivity and Adaptive Capacity",
                  y = "Modelled Response to Climate Change", fill = "Index",
                  shape = "Scenario")
}

