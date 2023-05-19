#' Plot scores for each vulnerability question
#'
#' A graph to help visualize what factors are influencing the index. Produces a
#' plotly graphic where the bars are the total score for the factor after
#' modifying it based on exposure. Hover to see the name of the factor and value
#' of the exposure multiplier. Facets are produced if there are multiple
#' scenarios
#'
#' @param vuln_df the \code{vuln_df} element of the result from
#'   \code{\link{calc_vulnerability}} with an added column for the
#'   scenario_name. See examples.
#'
#' @export
#'
#' @return plotly graph
#'
#' @examples
#'
#' # load dplyr for dealing with nested lists in table
#' library(dplyr)
#'
#' base_pth <- system.file("extdata", package = "ccviR")
#'
#' # scenario names
#' scn_nms <- c("RCP 4.5", "RCP 8.5")
#'
#' clim_vars <- get_clim_vars(file.path(base_pth, "clim_files/processed"), scn_nms)
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
#'
#' index_vuln <- calc_vulnerability(spat_res$spat_table, vuln, "Bird")
#'
#' bind_rows(index_vuln$vuln_df %>% `names<-`(index_vuln$scenario_name),
#'           .id = "scenario_name") %>%
#'   plot_q_score()

plot_q_score <- function(vuln_df){

  if(!"Question" %in% names(vuln_df)){
    vuln_df <- left_join(vuln_df, vulnq_code_lu_tbl, by = "Code")
  } else {
    vuln_df <- left_join(vuln_df, vulnq_code_lu_tbl, by = "Code") %>%
      rename(Question = "Question.y")
  }

  vuln_df <- mutate(vuln_df,
                    score = ifelse(.data$score <= 0, 0.001, .data$score),
                    custom_tooltip = paste0(.data$Question, ":\n",
                                            "Exposure Multiplier: ", .data$exp, "\n",
                                            "Score: ", round(.data$score, 2))) %>%
    filter(!is.na(.data$score))

  plt <- ggplot2::ggplot(vuln_df,
                         ggplot2::aes(x = .data$Code, y = .data$score, text = .data$custom_tooltip))+
    # added to make hover text work see https://github.com/plotly/plotly.R/issues/2114
    ggplot2::geom_point(size = 0.1, color = "grey35")+
    ggplot2::geom_col(color = "grey35")+
    ggplot2::facet_wrap(~scenario_name, ncol = 3)+
    ggplot2::labs(x = "Question", y = "Score")+
    ggplot2::scale_x_discrete(limits = rev)+
    ggplot2::coord_flip(expand = FALSE)

  plotly::ggplotly(plt, tooltip = "text")

  # Version with error bars for aggregated results

  # # for error bars need difference from mean
  # vuln_df <- filter(vuln_df, !is.na(score_mean)) %>%
  #   mutate(exp_min = exp_mean - exp_min,
  #          exp_max = exp_max - exp_mean,
  #          score_min = score_mean - score_min,
  #          score_max = score_max - score_mean)

  # plotly::plot_ly(vuln_df, hoverinfo = list("text", "y")) %>%
  #   plotly::add_markers(x = ~Code, y = ~exp_mean, name = "Exposure\nMultiplier",
  #               text = ~Question,
  #               marker = list(symbol = "x", size = 8),
  #               hovertemplate = "%{text}: <br> Exposure Multiplier: %{y}<extra></extra>",
  #               error_y = ~list(type = "data",
  #                               symmetric = FALSE,
  #                               array = exp_max,
  #                               arrayminus = exp_min,
  #                               thickness = 2)) %>%
  #   plotly::add_markers(x = ~Code, y = ~score_mean, name = "Score",
  #                       text = ~Question,
  #                       hovertemplate = "%{text}: <br> Score: %{y}<extra></extra>",
  #                       error_y = ~list(type = "data",
  #                                       symmetric = FALSE,
  #                                       array = score_max,
  #                                       arrayminus = score_min, thickness = 1)) %>%
  #   plotly::layout(xaxis = list(title = "Question"), yaxis = list(title = "Score"))
}



