#' Create bar indicator of index
#'
#' If min or max are missing use ind ie no error bars
#'
#' @param ind,ind_max,ind_min string index initials one of "IE", "EV", "HV",
#'   "MV", "LV"
#'
#' @noRd

plt_index_gauge <- function(ind, ind_min = ind, ind_max = ind,
                            method = "plotly"){
  df <- data.frame(val = 1,
                   ind = rep(c("IE", "EV", "HV", "MV", "LV"), 20) %>%
                     factor(levels = c("IE","LV", "MV", "HV", "EV")) %>%
                     sort())

  val_df <- data.frame(ind = ind, ind_min = ind_min, ind_max = ind_max) %>%
    mutate(name = factor(ind, levels = c("IE","LV", "MV", "HV", "EV"),
                             labels = c("Insufficient Evidence",
                                             "Less Vulnerable",
                                             "Moderately Vulnerable",
                                             "Highly Vulnerable",
                                             "Extremely Vulnerable")),
           col = case_when(ind == "IE" ~ "#808080",
                           ind == "EV" ~ "#FF0000",
                           ind == "HV" ~ "#FF8C00",
                           ind == "MV" ~ "#FFC125",
                           ind == "LV" ~ "#008000",
                           TRUE ~ "#808080"),
           ang = case_when(ind == "IE" ~ 18*pi/180,
                           ind == "EV" ~ 162*pi/180,
                           ind == "HV" ~ 126*pi/180,
                           ind == "MV" ~ 90*pi/180,
                           ind == "LV" ~ 54*pi/180,
                           TRUE ~ 18*pi/180),
           arrow_y = 0.38*sin(ang),
           arrow_x = sqrt(0.38^2 - arrow_y^2)) %>%
    mutate(val = 1,
           arrow_x = ifelse(ind %in% c("EV", "HV"), arrow_x*-1, arrow_x)) %>%
    mutate(across(.cols = contains("ind"),
                  .fns = ~case_when(.x == "IE" ~ 10,
                                    .x == "EV" ~ 90,
                                    .x == "HV" ~ 70,
                                    .x == "MV" ~ 50,
                                    .x == "LV" ~ 30,
                                    TRUE ~ 10)))

  print(val_df)

  if(method == "ggplot"){
    plt <- ggplot2::ggplot(df, ggplot2::aes(x = val, fill = ind))+
      ggplot2::geom_bar()+
      ggplot2::geom_hline(yintercept = val_df$ind, size = 2)+
      ggplot2::geom_errorbar(data = val_df,
                             ggplot2::aes(x = val, ymin = ind_min, ymax = ind_max),
                             inherit.aes = FALSE)+
      ggplot2::coord_flip()+
      ggplot2::scale_y_continuous(breaks = c(10, 30, 50, 70, 90),
                                  labels = c("Insufficient\nEvidence",
                                             "Less\nVulnerable",
                                             "Moderately\nVulnerable",
                                             "Highly\nVulnerable",
                                             "Extremely\nVulnerable"),
                                  name = NULL, expand = ggplot2::expansion())+
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(), labels = NULL,
                                  breaks = NULL, name = NULL)+
      ggplot2::scale_fill_discrete(type = rev(c("#808080", "#008000", "#FFC125", "#FF8C00",
                                                "#FF0000")), guide = "none")+
      ggplot2::theme_minimal()+
      ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA),
                     axis.text = ggplot2::element_text(size = 12))
    # Add this to make half pie chart
    # scale_y_continuous(..., limits = c(0,200))
    # coord_polar(theta ='y', start= -pi/2)
  }

  if(method == "plotly"){
    annot <- list(x = 0.5, y = 0.18, text = val_df$name,
                  showarrow = FALSE,
                  font = list(color = val_df$col, size = 16, face = "bold"))

    tickannot <- data.frame(x = c(0.25, 0.36, 0.5, 0.64, 0.75),
                            y = c(0.2, 0.8, 0.95, 0.8, 0.2),
                            name = c("Insufficient\nEvidence",
                                     "Less Vulnerable",
                                     "Moderately\nVulnerable",
                                     "Highly\nVulnerable",
                                     "Extremely\nVulnerable"),
                            code = c("IE","LV", "MV", "HV", "EV"),
                            col = c("#808080", "#008000", "#FFC125", "#FF8C00",
                                        "#FF0000"))

    plt <- plotly::plot_ly(domain = list(x = c(0, 1), y = c(0, 1)),
                           height = 300, width = 300)  %>%
      plotly::add_annotations(x = 0.5-val_df$arrow_x, y = 0.25+val_df$arrow_y ,
                              text = ".",
                              ax = 0.5, ay = 0.25,
                              xref='paper',
                              yref='paper',
                              axref='paper',
                              ayref='paper',
                              showarrow = TRUE)%>%
      # plotly::add_annotations(data = tickannot, x = ~x, y = ~y, text = ~code,
      #                         font = list(size = 0.1),
      #                         hovertext = ~name,
      #                         showarrow = FALSE) %>%
      plotly::add_trace(type = "indicator",
                        mode = "gauge",
                        gauge = list(
                          #shape = "bullet",
                          axis = list(range = c(0,100),
                                      ticks = "",
                                      showticklabels = FALSE,
                                      tickmode = "array",
                                      tickvals = c(10, 30, 50, 70, 90),
                                      ticktext = c("Insufficient\nEvidence",
                                                   "Less\nVulnerable",
                                                   "Moderately\nVulnerable",
                                                   "Highly\nVulnerable",
                                                   "Extremely\nVulnerable")),
                          # threshold = list(
                          #   line = list(width = 5),
                          #   thickness = 2,
                          #   value = val_df$ind),
                          bgcolor = "white",
                          steps = list(list(range = c(0, 20), color = "grey"),
                                       list(range = c(20, 40), color = "green"),
                                       list(range = c(40, 60), color = "#FFC125"),
                                       list(range = c(60, 80), color = "darkorange"),
                                       list(range = c(80, 100), color = "red"))
                        )) %>%
      plotly::config(displayModeBar = FALSE, displaylogo = FALSE,
                     staticPlot = FALSE) %>%
      plotly::layout(margin = list(l = 10,r = 10,b = 10,t = 10), annotations = annot)
  }
  return(plt)
}

plt_mig_exp_gauge <- function(ind, ind_min = ind, ind_max = ind, method = "plotly"){

  df <- data.frame(val = 1,
                   ind = rep(c("N/A", "Low", "Moderate", "High"), 25) %>%
                     factor(levels = c("N/A", "Low", "Moderate", "High")) %>%
                     sort())

  val_df <- data.frame(ind = ind, ind_min = ind_min, ind_max = ind_max) %>%
    mutate(name = ind,
           col = case_when(ind == "N/A" ~ "#808080",
                           ind == "EV" ~ "#FF0000",
                           ind == "HV" ~ "#FF8C00",
                           ind == "MV" ~ "#FFC125",
                           ind == "LV" ~ "#008000",
                           TRUE ~ "#808080")) %>%
    mutate(across(.fns = ~case_when(.x == "N/A" ~ 13.5,
                                    .x == "Low" ~ 38.5,
                                    .x == "Moderate" ~ 63.5,
                                    .x == "High" ~ 88.5,
                                    TRUE ~ 13.5))) %>%
    mutate(val = 1)

  if(method == "ggplot"){
    plt <- ggplot2::ggplot(df, ggplot2::aes(x = val, fill = ind))+
      ggplot2::geom_bar()+
      ggplot2::geom_hline(yintercept = val_df$ind, size = 2)+
      ggplot2::geom_errorbar(data = val_df,
                             ggplot2::aes(x = val, ymin = ind_min, ymax = ind_max),
                             inherit.aes = FALSE)+
      ggplot2::coord_flip()+
      ggplot2::scale_y_continuous(breaks = c(13.5 + 25*0:3),
                                  labels =  c("Not\nApplicable",
                                              "Low",
                                              "Moderate",
                                              "High"),
                                  name = NULL, expand = ggplot2::expansion())+
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(), labels = NULL,
                                  breaks = NULL, name = NULL)+
      ggplot2::scale_fill_discrete(type = rev(c("#808080", "#008000", "#FF8C00",
                                                "#FF0000")), guide = "none")+
      ggplot2::theme_minimal()+
      ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA),
                     axis.text = ggplot2::element_text(size = 12))
  }

  if(method == "plotly"){
    annot <- list(x = 0.5, y = 0.1, text = val_df$name,
                  showarrow = FALSE,
                  font = list(color = val_df$col, size = 20, face = "bold"))

    tickannot <- data.frame(x = c(0.25, 0.36, 0.5, 0.64),
                            y = c(0.2, 0.8, 0.95, 0.8),
                            name = c("Not\nApplicable",
                                     "Low",
                                     "Moderate",
                                     "High"),
                            code = c("N/A","L", "M", "H"),
                            col = c("#808080", "#008000", "#FF8C00",
                                    "#FF0000"),
                            breaks = c(13.5 + 25*0:3))

    plt <- plotly::plot_ly(domain = list(x = c(0, 1), y = c(0, 1)),
                           height = 200) %>%
      plotly::add_annotations(data = tickannot, x = ~x, y = ~y, text = ~code,
                              font = list(size = 0.1),
                              hovertext = ~name,
                              showarrow = FALSE) %>%
      plotly::add_trace(type = "indicator",
                        mode = "gauge",
                        gauge = list(
                          #shape = "bullet",
                          axis = list(range = c(0,100),
                                      ticks = "",
                                      showticklabels = FALSE,
                                      tickmode = "array",
                                      tickvals = tickannot$breaks,
                                      ticktext = tickannot$name),
                          threshold = list(
                            line = list(width = 5),
                            thickness = 2,
                            value = val_df$ind),
                          bgcolor = "white",
                          steps = list(list(range = c(0, 25), color = "grey"),
                                       list(range = c(25, 50), color = "green"),
                                       list(range = c(50, 75), color = "darkorange"),
                                       list(range = c(75, 100), color = "red"))
                        )) %>%
      plotly::config(displayModeBar = FALSE, displaylogo = FALSE,
                     staticPlot = FALSE) %>%
      plotly::layout(margin = list(r = 100, l = 100, t = 50), annotations = annot)

  }
  return(plt)
}



