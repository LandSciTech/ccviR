#' Plot bivariate exposure map
#'
#' Create a map of exposure to climate change based on both change in
#' temperature and change in climate moisture deficit.
#'
#' @param mat RasterLayer of classified mean annual temperature exposure.
#' @param cmd RasterLayer of classified climate moisture deficit exposure.
#' @param scale_poly sf polygon of the assessment area.
#' @param rng_poly sf polygon of the species range. Optional.
#' @param leg_rel_size numeric, shrinkage of the legend size relative to the
#'   plot. Default is 2.5 larger numbers will make the legend smaller
#' @param palette named vector of colours in each corner of the bivariate scale.
#'   Required names are bottomleft, bottomright, upperleft, and upperright.
#'
#' @return a list containing 2 ggplot objects "plot" containing the exposure map
#'   and "legend" containing the legend
#' @export
#'
#' @examples
#' # load the demo data
#' file_dir <- system.file("extdata", package = "ccviR")
#'
#' # scenario names
#' scn_nms <- c("RCP 4.5", "RCP 8.5")
#'
#' clim_vars <- get_clim_vars(file.path(file_dir, "clim_files/processed"),
#'                            scn_nms)
#'
#' mat <- clim_vars$mat$RCP_4.5
#' cmd <- clim_vars$cmd$RCP_4.5
#'
#' assess <- sf::st_read(file.path(file_dir, "assess_poly.shp"), agr = "constant",
#'                   quiet = TRUE)
#' rng <- sf::st_read(file.path(file_dir, "rng_poly.shp"), agr = "constant",
#'                quiet = TRUE)
#'
#' plot_bivar_exp(mat, cmd, assess, rng)
#'
plot_bivar_exp <- function(mat, cmd, scale_poly, rng_poly = NULL, leg_rel_size = 2.5,
                           palette = c(bottomleft = "green", bottomright = "blue",
                                       upperleft = "orange", upperright = "magenta")){
  col_mat <- colmat(6, bottomleft = palette["bottomleft"],
                    bottomright = palette["bottomright"],
                    upperleft = palette["upperleft"],
                    upperright = palette["upperright"], do_plot = F)

  bivar_ras <- bivar_map(cmd, mat)

  # using ggplot better than tmap for keeping the legend aligned well
  bivar_leg <- terra::rast(matrix(1:36, nrow = 6)) %>% terra::flip() %>%
    terra::as.data.frame(xy = TRUE) %>%
    ggplot2::ggplot()+
    ggplot2::geom_raster(ggplot2::aes(x = .data[["x"]], y = .data[["y"]],
                                      fill = as.factor(.data[["lyr.1"]])))+
    ggplot2::scale_fill_manual(values = as.vector(col_mat) |> setNames(1:36),
                               name = NULL, na.value = "white")+
    ggplot2::guides(fill = "none")+
    ggplot2::coord_equal()+
    ggplot2::labs(x = expression(Drier~symbol('\256')),
                  y = expression(Warmer~symbol('\256')))+
    ggplot2::theme(line = ggplot2::element_blank(), rect = ggplot2::element_blank(),
                   text = ggplot2::element_text(family = "", face = "plain",
                                                colour = "black", size = 9, lineheight = 0.9,
                                                hjust = 0.5, vjust = 0.5, angle = 0, margin = ggplot2::margin(),
                                                debug = FALSE), axis.text = ggplot2::element_blank(),
                   axis.ticks.length = ggplot2::unit(0, "pt"))

  ras_ext <- terra::ext(bivar_ras)

  rast_df <- terra::as.data.frame(bivar_ras, xy = TRUE)


  rast_df <- rast_df %>% tidyr::pivot_longer(names(bivar_ras),
                                             names_to = "layer_name",
                                             values_to = "value")


  leg_start_x <- ras_ext$xmax + (ras_ext$xmax - ras_ext$xmin)/20
  leg_end_x <- ras_ext$xmax + (ras_ext$xmax - ras_ext$xmin)/leg_rel_size
  leg_top_y <- ras_ext$ymax - (ras_ext$ymax - ras_ext$ymin)/20
  leg_bottom_y <-  ras_ext$ymax - (ras_ext$xmax - ras_ext$xmin)/leg_rel_size

  bivar_plt <- ggplot2::ggplot()+
    ggplot2::geom_raster(ggplot2::aes(x = .data[["x"]], y = .data[["y"]],
                                      fill = as.factor(.data[["value"]])),
                         data = rast_df)+
    ggplot2::geom_sf(data = scale_poly, fill = NA, col = "black")+
    ggplot2::theme_void()+
    ggplot2::scale_fill_manual(values = as.vector(col_mat) |> setNames(1:36),
                               name = NULL, na.value = "white")+
    ggplot2::guides(fill = "none")
    # ggplot2::theme(plot.margin = ggplot2::unit(c(0.01,0.333,0.01,0.01), "npc"))
    # ggplot2::annotation_custom(ggplot2::ggplotGrob(bivar_leg),
    #                            xmin = leg_start_x, xmax = leg_end_x,
    #                            ymin = leg_bottom_y, ymax = leg_top_y)

  if(!is.null(rng_poly)){
    bivar_plt <- bivar_plt+
      ggplot2::geom_sf(data = rng_poly, fill = NA, col = "black", linewidth = 1)
  }

  if(terra::nlyr(bivar_ras) > 1){
    bivar_plt <- bivar_plt + ggplot2::facet_wrap(~layer_name)
  }

  list(plot = bivar_plt, legend = bivar_leg)
}

# derived from this blog post https://rfunctions.blogspot.com/2015/03/bivariate-maps-bivariatemap-function.html
colmat <- function(nclass = 6,
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
    terra::rast(matrix(1:nclass^2, nrow = nclass)) %>% terra::flip() %>%
      terra::plot(col = as.vector(col.matrix), frame.plot = F, axes = F, box = F,
           add = F, legend = F)
  }

 col.matrix
}


bivar_map <- function(rasterx, rastery, nclass = 6) {

  col_grid <- expand.grid(1:nclass, 1:nclass) %>%
    mutate(Var1 = .data$Var1 *10,
           Var3 = .data$Var1 + .data$Var2,
           value = 1:n()) %>%
    dplyr::select("Var3", "value")

  classify_col <- Vectorize(function(x, y) {
    col_grid$value[which(col_grid$Var1 == x & col_grid$Var2 == y)]
  })

  r <- terra::classify(rasterx + rastery*10, rcl = col_grid)
  names(r) <- names(rasterx)
  return(r)
}


