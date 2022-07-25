#' Create bar indicator of index
#'
#' If min or max are missing use ind ie no error bars
#'
#' @param ind string. index value matching one of \code{codes}
#' @param type string. Type of index either "index" or "mig_exp". Will set default
#'   colours and labels. If FALSE codes, nms and cols are use.
#' @param codes character. Options for ind. Should be short 1-3 letters
#' @param nms character. Long form names to go with codes
#' @param cols character. Colours to use for each code
#' @param ttl character. Title for the plot
#'
#' @noRd

plt_index_gauge <- function(ind, type, codes, nms, cols, ttl){

  if(type == "index"){
    codes <- c("IE","LV", "MV", "HV", "EV")
    nms <- c("Insufficient Evidence",
               "Less Vulnerable",
               "Moderately Vulnerable",
               "Highly Vulnerable",
               "Extremely Vulnerable")
    cols <- c("#808080", "#008000", "#FFC125", "#FF8C00",
              "#FF0000")
    ttl <- "Climate Change Vulnerability Index"
  } else if(type == "mig_exp") {
    codes <- c("N/A", "L", "M", "H")
    nms <- c("N/A", "Low", "Moderate", "High")
    cols <- c("#808080", "#008000", "#FFC125",
              "#FF0000")
    if(ind %in% nms){
      ind <- codes[which(ind == nms)]
    }
    ttl <- "Migratory Exposure Index"
  }
  # parameters related to annotation placement
  # inner circle radius
  r1 <- 0.37

  # middle of arch circle radius
  r2 <- 0.43

  # Number of levels in index
  n <- length(codes)

  # angles for calculating location of arrow/text in rads
  angles <- seq(from = 180/(n*2),
                to = 180/(n*2)+180/n*(n-1),
                by = 180/n)*pi/180

  # circle centre
  cent_x <- 0.5
  cent_y <- 0.0

  tickannot <- tibble(name = nms,
                      code = codes,
                      col = cols,
                      text_ang = angles,
                      text_y = r2*sin(.data$text_ang),
                      text_x = sqrt(r2^2 - .data$text_y^2),
                      gauge_vals = seq(from = 100/n,
                                       to = 100, by = 100/n)) %>%
    mutate(text_x = ifelse(.data$text_ang > 90*pi/180, cent_x + .data$text_x,
                           cent_x - .data$text_x),
           text_y = cent_y + 2*.data$text_y)


  val_df <- data.frame(code = ind) %>%
    left_join(tickannot, by = "code") %>%
    mutate(arrow_y = r1*sin(.data$text_ang),
           arrow_x = sqrt(r1^2 - .data$arrow_y^2)) %>%
    mutate(arrow_x = ifelse(.data$text_ang > 90*pi/180, .data$arrow_x*-1, .data$arrow_x),
           arrow_y = .data$arrow_y*2)

  annot <- list(x = cent_x, y = cent_y , text = val_df$name,
                showarrow = FALSE,
                font = list(color = val_df$col, size = 16, face = "bold"),
                bgcolor = "white",
                xanchor = "center",
                yanchor = "middle")

  #Note changing ht or wd will mess up placement of annotations

  plt <- plotly::plot_ly(domain = list(x = c(0, 1), y = c(0, 1)),
                         height = 190, width = 300)  %>%
    plotly::add_annotations(x = cent_x-val_df$arrow_x, y = cent_y+val_df$arrow_y ,
                            text = ".",
                            ax = cent_x, ay = cent_y,
                            xref='paper',
                            yref='paper',
                            axref='paper',
                            ayref='paper',
                            showarrow = TRUE)%>%
    plotly::add_annotations(data = tickannot, x = ~text_x, y = ~text_y,
                            text = ~code,
                            font = list(size = 0.1),
                            hovertext = ~name,
                            showarrow = FALSE,
                            xanchor = "center",
                            yanchor = "middle") %>%
    plotly::add_trace(type = "indicator",
                      mode = "gauge",
                      gauge = list(
                        axis = list(range = c(0,100),
                                    ticks = "",
                                    showticklabels = FALSE),
                        bgcolor = "white",
                        steps = purrr::pmap(list(c(0, tickannot$gauge_vals[1:n-1]),
                                                 tickannot$gauge_vals,
                                                 tickannot$col),
                                            ~list(range = c(..1, ..2), color = ..3))
                      )) %>%
    plotly::config(displayModeBar = FALSE, displaylogo = FALSE,
                   staticPlot = FALSE) %>%
    plotly::layout(title = list(text = ttl), margin = list(l = 10,r = 10,b = 10,t = 40), annotations = annot)

  return(plt)
}

