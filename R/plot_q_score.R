#' Plot scores for each vulnerability question
#'
#' A graph to help visualize what factors are influencing the index. Produces a plotly
#' graphic where the bars are the total score for the factor after modifying it
#' based on exposure and the dots are the exposure multiplier. Hover to see the
#' name of the factor.
#'
#' @param vuln_df the \code{vuln_df} element of the result from
#'   \code{\link{calc_vulnerability}}.
#'
#' @export
#'
#' @return plotly graph
#'
#' @examples
#' base_pth <- system.file("extData", package = "ccviR")
#'
#' clim_vars <- get_clim_vars(file.path(base_pth, "clim_files/processed"))
#'
#' spat_res <- run_spatial(
#'   range_poly = sf::read_sf(file.path(base_pth, "rng_poly_high.shp"), agr = "constant"),
#'   scale_poly = sf::read_sf(file.path(base_pth, "assess_poly.shp"), agr = "constant"),
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
#' plot_q_score(index_vuln$vuln_df)

plot_q_score <- function(vuln_df){

  if(!"Question" %in% names(vuln_df)){
    vuln_df <- left_join(vuln_df, ccviR::vulnq_code_lu_tbl, by = "Code")
  } else {
    vuln_df <- left_join(vuln_df, ccviR::vulnq_code_lu_tbl, by = "Code") %>%
      rename(Question = "Question.y")
  }

  vuln_df <- filter(vuln_df, !Code %in% c("Z2", "Z3")) %>%
    mutate(score = case_when(score < 0 ~ 0,
                             is.na(score) ~ 0,
                             TRUE ~ score))

  plotly::plot_ly(vuln_df, hoverinfo = list("text", "y")) %>%
    plotly::add_bars(x = ~Code, y = ~score, name = "Score",
             text = ~Question,
             hovertemplate = "%{text}: <br> Score: %{y}<extra></extra>") %>%
    plotly::add_markers(x = ~Code, y = ~exp, name = "Exposure\nMultiplier",
                text = ~Question,
                hovertemplate = "%{text}: <br> Exposure Multiplier: %{y}<extra></extra>") %>%
    plotly::layout(xaxis = list(title = "Question"), yaxis = list(title = "Score"))
}



