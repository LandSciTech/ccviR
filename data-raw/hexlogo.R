## code to prepare `hexlogo` dataset goes here
library(dplyr)

# temp file
temp_fn <- tempfile()

ind <- "HV"

codes <- c("IE","LV", "MV", "HV", "EV")
nms <- c("Insufficient Evidence",
         "Less Vulnerable",
         "Moderately Vulnerable",
         "Highly Vulnerable",
         "Extremely Vulnerable")
cols <- c("#808080", "#008000", "#FFC125", "#FF8C00",
          "#FF0000")

# parameters related to annotation placement
# inner circle radius
r1 <- 0.37

# middle of arch circle radius
r2 <- 0.43

# Number of levels in index
n <- length(codes)

#Note changing ht or wd will mess up placement of annotations

plt <- plotly::plot_ly(domain = list(x = c(0, 1), y = c(0, 1)),
                       height = 190, width = 300)  %>%
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
  plotly::layout(margin = list(l = 10,r = 10,b = 10,t = 40))
plt


# exported a picture manually

mp <- png::readPNG("logo_temp.png")

mp <- grid::rasterGrob(mp, interpolate=TRUE)
s <- hexSticker::sticker(mp, package = "ccviR",
                         p_size = 18, # This seems to behave very differently on a Mac vs PC
                         p_y = 0.8,
                         p_color = "black",
                         s_x = 1, s_y = 1,
                         s_width = 1.5, s_height = 2,
                         h_fill = "white",
                         h_color = "black",
                         white_around_sticker = TRUE,
                         filename = paste0(temp_fn, ".png"))

plot(s)

usethis::use_logo(paste0(temp_fn, ".png"))
