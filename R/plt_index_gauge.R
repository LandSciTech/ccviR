#' Create bar indicator of index
#'
#'
#' @param ind string index initials one of "IE", "EV", "HV", "MV", "LV"
#' @param ht number, height of the bar. default is 150
#'
#' @noRd

plt_index_gauge <- function(ind, ht = 100){
  val <- case_when(ind == "IE" ~ 10,
                   ind == "EV" ~ 90,
                   ind == "HV" ~ 70,
                   ind == "MV" ~ 50,
                   ind == "LV" ~ 30,
                   TRUE ~ 10)
  plotly::plot_ly(
    type = "indicator",
    mode = "gauge",
    domain = list(x = c(0, 1), y = c(0, 1)),
    gauge = list(
      shape = "bullet",
      axis = list(range = c(0,100),
                  ticks = "",
                  tickmode = "array",
                  tickvals = c(10, 30, 50, 70, 90),
                  ticktext = c("Insufficient\nEvidence",
                               "Less\nVulnerable",
                               "Moderately\nVulnerable",
                               "Highly\nVulnerable",
                               "Extremely\nVulnerable")),
      threshold = list(
        line = list(width = 5),
        thickness = 2,
        value = val),
      bgcolor = "white",
      steps = list(list(range = c(0, 20), color = "grey"),
                   list(range = c(20, 40), color = "green"),
                   list(range = c(40, 60), color = "#FFC125"),
                   list(range = c(60, 80), color = "darkorange"),
                   list(range = c(80, 100), color = "red"))
    ),
    height = ht) %>%
    plotly::config(displayModeBar = FALSE, displaylogo = FALSE,
                   staticPlot = TRUE)
}

plt_mig_exp_gauge <- function(ind, ht = 100){
  val <- case_when(ind == "N/A" ~ 13.5,
                   ind == "Low" ~ 38.5,
                   ind == "Moderate" ~ 63.5,
                   ind == "High" ~ 88.5,
                   TRUE ~ 13.5)
  plotly::plot_ly(
    type = "indicator",
    mode = "gauge",
    domain = list(x = c(0, 1), y = c(0, 1)),
    gauge = list(
      shape = "bullet",
      axis = list(range = c(0,100),
                  ticks = "",
                  tickmode = "array",
                  tickvals = c(13.5 + 25*0:3),
                  ticktext = c("Not\nApplicable",
                               "Low",
                               "Moderate",
                               "High")),
      threshold = list(
        line = list(width = 5),
        thickness = 2,
        value = val),
      bgcolor = "white",
      steps = list(list(range = c(0, 25), color = "grey"),
                   list(range = c(25, 50), color = "green"),
                   list(range = c(50, 75), color = "darkorange"),
                   list(range = c(75, 100), color = "red"))
    ),
    height = ht) %>%
    plotly::config(displayModeBar = FALSE, displaylogo = FALSE,
                   staticPlot = TRUE)
}
