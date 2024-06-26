% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_q_score.R
\name{plot_q_score}
\alias{plot_q_score}
\title{Plot scores for each vulnerability question}
\usage{
plot_q_score(vuln_df)
}
\arguments{
\item{vuln_df}{the \code{vuln_df} element of the result from
\code{\link{calc_vulnerability}} with an added column for the
scenario_name. See examples.}
}
\value{
plotly graph
}
\description{
A graph to help visualize what factors are influencing the index. Produces a
plotly graphic where the bars are the total score for the factor after
modifying it based on exposure. Hover to see the name of the factor and value
of the exposure multiplier. Facets are produced if there are multiple
scenarios
}
\examples{

# load dplyr for dealing with nested lists in table
library(dplyr)

base_pth <- system.file("extdata", package = "ccviR")

# scenario names
scn_nms <- c("RCP 4.5", "RCP 8.5")

clim_vars <- get_clim_vars(file.path(base_pth, "clim_files/processed"), scn_nms)

spat_res <- analyze_spatial(
  range_poly = sf::read_sf(file.path(base_pth, "rng_poly.shp"), agr = "constant"),
  scale_poly = sf::read_sf(file.path(base_pth, "assess_poly.shp"), agr = "constant"),
  clim_vars_lst = clim_vars,
  hs_rast = terra::rast(c(file.path(base_pth, "rng_chg_45.tif"),
                          file.path(base_pth, "rng_chg_85.tif"))),
  hs_rcl = matrix(c(-1, 0, 1, 1, 2, 3), ncol = 2),
  scenario_names = scn_nms
)

# vulnerability factor table with score 1 (somewhat increase vulnerability)
# for all factors
vuln <- make_vuln_df("test_species", val1 = 1, mig = 1)

index_vuln <- calc_vulnerability(spat_res$spat_table, vuln, "Bird")

bind_rows(index_vuln$vuln_df \%>\% `names<-`(index_vuln$scenario_name),
          .id = "scenario_name") \%>\%
  plot_q_score()
}
