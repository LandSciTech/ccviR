plot_spat_res <- function(mat, cmd){
  col_mat <- colmat(6, bottomleft = "green", bottomright = "blue",
                    upperleft = "orange", upperright = "magenta", do_plot = F)
  bivar_map <- bivar_map(cmd, mat)
  raster::plot(bivar_map, col = as.vector(col_mat), legend = FALSE, axes = FALSE, box = FALSE)
}


colmat <- function(nclass = 10,
                   upperleft = rgb(0, 150, 235, maxColorValue = 255),
                   upperright = rgb(130, 0, 80, maxColorValue = 255),
                   bottomleft = "grey",
                   bottomright = rgb(255, 230, 15, maxColorValue = 255),
                   xlab = "x label", ylab = "y label", do_plot = FALSE) {
  my.pal.1 <- colorRampPalette(c(upperleft, bottomleft))(nclass)
  my.pal.2 <- colorRampPalette(c(upperright, bottomright))(nclass)
  col.matrix <- matrix(nrow = nclass, ncol = nclass, NA)
  for (i in 1:nclass) {
    my.col <- c(paste(my.pal.1[i]), paste(my.pal.2[i]))
    col.matrix[nclass +1 - i, ] <- colorRampPalette(my.col)(nclass)
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
    mutate(Var1 = Var1 *10,
           Var3 = Var1 + Var2,
           value = 1:n()) %>%
    dplyr::select(Var3, value)

  classify_col <- Vectorize(function(x, y) {
    col_grid$value[which(col_grid$Var1 == x & col_grid$Var2 == y)]
  })

  r <- raster::reclassify(rasterx + rastery*10, rcl = col_grid)
}


