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
                                            "Exposure Multiplier: ", round(.data$exp, 2), "\n",
                                            "Score: ", round(.data$score, 2))) %>%
    filter(!is.na(.data$score)) %>%
    mutate(sub_index = ifelse(startsWith(.data$Code, "D"), "D index", "B/C index"))

  # get maximum scores for each question
  max_df <- make_vuln_df("test") %>%
    mutate(Value1 = as.numeric(.data$Max_Value)) %>%
    calc_vuln_score(spat_df = data.frame(temp_exp_cave = 2.4,
                                         moist_exp_cave = 2,
                                         comb_exp_cave = mean(c(2.4, 2)))) %>%
    filter(!is.na(.data$score)) %>%
    mutate(sub_index = ifelse(startsWith(.data$Code, "D"), "D index", "B/C index"),
           section = stringr::str_extract(.data$Code, "^.") %>% as.factor())

  #plotly doesn't respect space = free so need to make subplots individually see
  #https://github.com/plotly/plotly.R/issues/908

  # define plot yaxis limits
  limits <- max_df %>%
    summarise(max = ceiling(max(.data$score)),
              min = floor(min(.data$score)))

  #define width of subplots by finding the absolut range of each "facet"
  plot_width<- max_df %>%
    group_by(.data$sub_index) %>%
    summarise(range = n()) %>%
    mutate(width_pct = ifelse(.data$sub_index == "D index", 0.02 + range/sum(range),
                              -0.02 + range/sum(range)))

  # add colors by section
  cols_use <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3')
  cols_use <- c(B = cols_use[5], C = cols_use[1], D = cols_use[4])

  #define a list of ggplot and feed it in the subplot function with the calculated limits
  vuln_df <- vuln_df %>% right_join(vulnq_code_lu_tbl) %>%
    mutate(section = stringr::str_extract(.data$Code, "^.") %>% as.factor())

  max_lst <- rep(split(max_df, max_df$sub_index), n_distinct(vuln_df$scenario_name, na.rm = TRUE))

  plt_lst <- split(vuln_df, list(vuln_df$scenario_name, vuln_df$sub_index)) %>%
    purrr::map2(
      #Reorder with odd and even elements
      c(max_lst[c(TRUE, FALSE)],
        max_lst[c(FALSE, TRUE)]),
      function(x, y) {
        plt <- ggplot2::ggplot(
          x,
          ggplot2::aes(x = .data$Code, y = .data$score, text = .data$custom_tooltip,
                       fill = .data$section)
        )+
          # added to make hover text work see https://github.com/plotly/plotly.R/issues/2114
          ggplot2::geom_point(size = 0.1, color = "grey35")+
          ggplot2::geom_col(color = "grey35")+
          ggplot2::scale_fill_discrete(type = cols_use, drop = FALSE)+
          ggplot2::geom_col(ggplot2::aes(x = .data$Code, y = .data$score, fill = .data$section),
                            data = y, alpha = 0.4,
                            inherit.aes = FALSE)+
          ggplot2::facet_grid(sub_index ~ scenario_name, scales = "free")+
          ggplot2::labs(x = "Question", y = "Score")+
          ggplot2::scale_x_discrete(limits = rev)+
          ggplot2::coord_flip(expand = FALSE, ylim = c(0, limits$max))+
          ggplot2::theme(legend.position = "none")
        plotly::ggplotly(plt, tooltip = "text")
      })
  plt_lst %>%
    plotly::subplot(margin = 0.02, nrows = 2, shareY = TRUE, shareX = TRUE,
                    heights = plot_width$width_pct)


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



