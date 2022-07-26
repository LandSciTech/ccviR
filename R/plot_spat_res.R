plot_spat_res <- function(mat, cmd){
  col_mat <- colmat(6, bottomleft = "green", bottomright = "blue",
                    upperleft = "orange", upperright = "magenta", do_plot = F)
  bivar_ras <- bivar_map(cmd, mat)

  bivar_out <- tmap::qtm(bivar_ras, raster.palette = as.vector(col_mat) |> setNames(1:36),
                         raster.style = "cat", layout.legend.show = FALSE,
                         layout.inner.margins = c(0.3, 0, 0, 0))

  bivar_leg <- raster(matrix(1:36, nrow = 6)) %>% raster::flip() %>%
    raster::`projection<-`("EPSG:4326") |>
    tmap::qtm(raster.palette = as.vector(col_mat) |> setNames(1:36),
              raster.style = "cat", layout.legend.show = FALSE, layout.frame = FALSE,
              layout.inner.margins = c(0.1, 0.1, 0, 0))

  # not quite working
  cowplot::ggdraw()+
    cowplot::draw_grob(tmap::tmap_grob(bivar_out))+
    cowplot::draw_grob(tmap::tmap_grob(bivar_leg), height =  0.3, valign = 1)+
    cowplot::draw_label(expression(Moisture ~ Exposure~symbol('\256')), y = 0, vjust = 0, size = 10)+
    cowplot::draw_label(expression(Temperature ~ Exposure~symbol('\256')), y = 0, x = 0.39,
                        vjust = 0, hjust = 0, angle = 90, size = 10)
}


colmat <- function(nclass = 10,
                   upperleft = grDevices::rgb(0, 150, 235, maxColorValue = 255),
                   upperright = grDevices::rgb(130, 0, 80, maxColorValue = 255),
                   bottomleft = "grey",
                   bottomright = grDevices::rgb(255, 230, 15, maxColorValue = 255),
                   xlab = "x label", ylab = "y label", do_plot = FALSE) {
  my.pal.1 <- grDevices::colorRampPalette(c(upperleft, bottomleft))(nclass)
  my.pal.2 <- grDevices::colorRampPalette(c(upperright, bottomright))(nclass)
  col.matrix <- matrix(nrow = nclass, ncol = nclass, NA)
  for (i in 1:nclass) {
    my.col <- c(paste(my.pal.1[i]), paste(my.pal.2[i]))
    col.matrix[nclass +1 - i, ] <- grDevices::colorRampPalette(my.col)(nclass)
  }
  if(do_plot){
    raster(matrix(1:36, nrow = 6)) %>% raster::flip() %>%
      raster::plot(col = as.vector(col.matrix), frame.plot = F, axes = F, box = F,
           add = F, legend = F)
  }

 col.matrix
}


bivar_map <- function(rasterx, rastery, nclass = 6) {

  col_grid <- expand.grid(1:nclass, 1:nclass) %>%
    mutate(Var1 = .data$Var1 *10,
           Var3 = .data$Var1 + .data$Var2,
           value = 1:n()) %>%
    dplyr::select(.data$Var3, .data$value)

  classify_col <- Vectorize(function(x, y) {
    col_grid$value[which(col_grid$Var1 == x & col_grid$Var2 == y)]
  })

  r <- raster::reclassify(rasterx + rastery*10, rcl = col_grid)
}


