#' Create leaflet map
#'
#' Function to make maps
#'
#' @param poly1 Sf Polygon. Primary polygon to plot.
#' @param rast1 SpatRaster. Primary raster to plot.
#' @param poly2 Sf Polygon. Secondary polygon to plot.
#' @param rast2 SpatRaster. Secondary raster to plot.
#' @param poly1_nm Character. Identifier.
#' @param poly2_nm Character. Identifier.
#' @param rast1_nm Character. Identifier.
#' @param rast2_nm Character. Identifier.
#' @param rast1_lbl Character. Raster layer labels. Only for primary raster.
#' @param rast_grp Character. Raster layer groups.
#' @param max_cell Numeric. Maximum number of cells in a raster before
#'   aggregating for plotting.
#'
#' @returns Leaflet map
#' @noRd

make_map <- function(poly1, rast1 = NULL, poly2 = NULL, rast2 = NULL,
                     poly1_nm = "Current Range", poly2_nm = NULL,
                     rast1_nm = NULL, rast2_nm = NULL,
                     rast1_lbl = NULL,
                     rast_grp = NULL, max_cell = 5000000) {

  # Name of input data layers for mapping
  rast_nms <- list(`Temperature exposure class` = "mat",
                   `Historical precipitation (mm)` = "map",
                   `Moisture class` = "cmd",
                   `Climate change exposure index` = "ccei",
                   `Historical thermal niche` = "htn",
                   `Modeled range change` = "hs_rast",
                   `Protected areas` = "protected_rast")

  poly_nms <- list(`Assessment area`= "assess_poly",
                   `Non-breeding range` = "nonbreed_poly",
                   `Physiological thermal niche` = "ptn")

  if(!is.null(rast1_nm)) {
    if(rast1_nm == "hs_rast"){
      pal1 = c("grey", "#FF0000", "#FFC125", "#008000")
      brks = 0:3
      rast_vals <- terra::unique(rast1, incomparables = TRUE) %>% unlist() %>%
        unique()
      rast1_lbl <- bind_cols(rast1_lbl, pal = pal1) %>%
        filter(.data$value %in% rast_vals)
      pal1 <- rast1_lbl$pal
      col_tbl <- data.frame(value = rast1_lbl$value, col = pal1)
      for(l in 1:terra::nlyr(rast1)){
        terra::coltab(rast1, layer = l) <- col_tbl
      }
      rast1_lbl <- pull(rast1_lbl, .data$label)
    } else if(rast1_nm %in% c("cmd", "mat")) {
      pal1 = c("#FFF9CA", "#FEE697", "#FEC24D", "#F88B22", "#D85A09", "#A33803")
      rast1_lbl <- as.character(1:6)
      col_tbl <- data.frame(value = 1:6, col = pal1)
      for(l in 1:terra::nlyr(rast1)){
        terra::coltab(rast1, layer = l) <- col_tbl
      }
      # add descriptor to class label
      rast1_lbl[1] <- paste0(rast1_lbl[1], " - Low")
      rast1_lbl[length(rast1_lbl)] <- paste0(rast1_lbl[length(rast1_lbl)], " - High")
    } else if(rast1_nm %in% c("ccei", "htn")) {
      pal1 <- c("#FFF7BD", "#FECF66", "#F88B22", "#CC4C02")
      rast1_lbl <- as.character(1:4)
      # add descriptor to class label
      rast1_lbl[1] <- paste0(rast1_lbl[1], " - Low")
      rast1_lbl[length(rast1_lbl)] <- paste0(rast1_lbl[length(rast1_lbl)], " - High")
      col_tbl <- data.frame(value = 1:4, col = pal1)
      for(l in 1:terra::nlyr(rast1)){
        terra::coltab(rast1, layer = l) <- col_tbl
      }
    } else if(rast1_nm == "protected_rast") {
      if(rast2_nm != "hs_rast" || is.null(rast2)) {
        stop("need future ranges for plotting protected areas", call. = FALSE)
      }
      pal1 <- c("#3C7F3C")
      pal2 <- "#410E48"
      rast2 <- terra::subst(rast2, c(0, 1), NA) # Remove non-range values
    } else if(rast1_nm == "map"){
      rng_val <- terra::minmax(rast1)[,1]
      pal1 <- leaflet::colorNumeric("Blues", domain = rng_val, na.color = "#00000000")
    } else {
      stop("no match for rast1_nm")
    }
  } else{
    if(!is.null(rast1)){
      stop("rast1_nm must be provided if rast1 is not NULL", call. = FALSE)
    }
  }

  # tried adding a line break to legend but doesn't work in interactive map
  poly2_nm <- names(poly_nms)[which(poly_nms == poly2_nm)]
  rast1_nm <- names(rast_nms)[which(rast_nms == rast1_nm)]
  rast2_nm <- names(rast_nms)[which(rast_nms == rast2_nm)]

  rast1 <- prep_raster_map(rast1, rast1_nm, max_cell)
  rast2 <- prep_raster_map(rast2, rast2_nm, max_cell)

  rast_grp <- character(0)
  if(!is.null(rast1) && terra::nlyr(rast1) > 1) rast_grp <- names(rast1)
  if(!is.null(rast2) && terra::nlyr(rast2) > 1) rast_grp <- names(rast2)

  extra_pal <- NULL
  extra_labs <- NULL

  out <- leaflet::leaflet() %>%
    # Tiles first to go under rasters
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap") %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "CartoDB")

  # Add second Raster
  if(!is.null(rast2)) {
    for(l in 1:terra::nlyr(rast2)){
      out <- leaflet::addRasterImage(out, x = rast2[[l]], method = "ngb",
                                     colors = pal2,
                                     group = rast_grp[l], opacity = 1)
    }
    extra_pal <- c(extra_pal, pal2)
    extra_labs <- c(extra_labs, rast2_nm)
  }

  # Add primary Raster
  if(!is.null(rast1)) {
    for(l in 1:terra::nlyr(rast1)){
      out <- leaflet::addRasterImage(out, x = rast1[[l]], method = "ngb",
                                     colors = pal1,
                                     group = rast_grp[l], opacity = 1)
    }

    if(is.character(pal1)) { # If character palette (categorical)
      # If just one colour add to existing legend
      if(length(pal1) == 1) {
        if(is.null(rast1_lbl)) rast1_lbl <- rast1_nm
        extra_pal <- c(extra_pal, pal1)
        extra_labs <- c(extra_labs, rast1_lbl)
      } else {
        out <- leaflet::addLegend(out, colors = pal1, labels = rast1_lbl,
                                  title = rast1_nm, opacity = 1)
      }
    } else { # If palette function (continuous)
      out <- leaflet::addLegend(out, pal = pal1, values = rng_val[1]:rng_val[2],
                                title = rast1_nm, opacity = 1)
    }
  }

  # Add second polygon
  if(!is.null(poly2)) {
    out <- out %>%
      leaflet::addPolylines(data = poly2 %>% sf::st_transform(4326), color = "blue")
    extra_pal <- c(extra_pal, "blue")
    extra_labs <- c(extra_labs, poly2_nm)
  }

  # Add primary polygon and one-off legend elements
  extra_pal <- c(extra_pal, "black")
  extra_labs <- c(extra_labs, poly1_nm)
  out <- out %>%
    leaflet::addPolylines(data = poly1 %>% sf::st_transform(4326), color = "black") %>%
    leaflet::addLegend(colors = extra_pal, labels = extra_labs, opacity = 1)

  out <- out %>%
    leaflet::addLayersControl(
      baseGroups = c("CartoDB", "OpenStreetMap"),
      overlayGroups = rast_grp,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  return(out)
}

#' Make raster map
#'
#' Used for MAP, MAT, and historical thermal and hydrological niches
#'
#' @param poly Range polygon
#' @param rast Raster
#' @param rast_nm ID of raster type
#' @param max_cell Maximum cells in plotted raster
#'
#' @returns Leaflet map
#' @noRd
#'
#' @examples
#' d <- test_data()
#'
#' # Section A
#' map_rasters(poly = d$rng_poly, rast = d$clim_vars$mat, rast_nm = "mat")
#' map_rasters(poly = d$rng_poly, rast = d$clim_vars$cmd, rast_nm = "cmd")
#'
#' # Section C
#' map_rasters(poly = d$rng_poly, rast = d$clim_vars$htn, rast_nm = "htn")
#' map_rasters(poly = d$rng_poly, rast = d$clim_vars$map, rast_nm = "map")

map_rasters <- function(poly, rast, rast_nm, max_cell = 5000000) {

  if(rast_nm %in% c("cmd", "mat")) {
    pal = c("#FFF9CA", "#FEE697", "#FEC24D", "#F88B22", "#D85A09", "#A33803")
    rast_lbl <- as.character(1:6)
    col_tbl <- data.frame(value = 1:6, col = pal)
    for(l in 1:terra::nlyr(rast)){
      terra::coltab(rast, layer = l) <- col_tbl
    }
    # add descriptor to class label
    rast_lbl[1] <- paste0(rast_lbl[1], " - Low")
    rast_lbl[length(rast_lbl)] <- paste0(rast_lbl[length(rast_lbl)], " - High")
  } else if(rast_nm %in% c("ccei", "htn")) {
    pal <- c("#FFF7BD", "#FECF66", "#F88B22", "#CC4C02")
    rast_lbl <- as.character(1:4)
    # add descriptor to class label
    rast_lbl[1] <- paste0(rast_lbl[1], " - Low")
    rast_lbl[length(rast_lbl)] <- paste0(rast_lbl[length(rast_lbl)], " - High")
    col_tbl <- data.frame(value = 1:4, col = pal)
    for(l in 1:terra::nlyr(rast)){
      terra::coltab(rast, layer = l) <- col_tbl
    }
  } else if(rast_nm == "map"){
    rng_val <- terra::minmax(rast)[,1]
    pal <- leaflet::colorNumeric("Blues", domain = rng_val, na.color = "#00000000")
  } else {
    stop("no match for rast_nm")
  }

  rast_nm <- pretty_names(rast_nm)
  poly_nm <- pretty_names("range_poly")

  rast <- prep_raster_map(rast, rast_nm, max_cell)

  if(terra::nlyr(rast) > 1) rast_grp <- names(rast) else rast_grp <- character(0)

  # Map
  out <- layer_base() %>%
    layer_raster(rast, rast_nm, rast_lbl, pal, rast_grp, rng_val) %>%
    leaflet::addPolylines(data = sf::st_transform(poly, 4326), color = "black") %>%
    layer_extras("black", poly_nm, rast_grp)

  return(out)
}

#' Make basic polygon plots
#'
#' Used for creating the basic polygon plots
#'
#' @param poly1 Range polygon
#' @param poly2 Assessment area polygon
#'
#' @returns Leaflet map
#' @noRd
#'
#' @examples
#' d <- test_data()
#' map_poly(poly1 = d$rng_poly, poly2 = d$ptn)

map_poly <- function(poly1, poly2) {

  # Names
  poly1_nm <- pretty_names("range_poly")
  poly2_nm <- pretty_names("ptn")

  # Base Map
  out <- layer_base() %>%
    leaflet::addPolylines(data = sf::st_transform(poly1, 4326), color = "black") %>%
    leaflet::addPolylines(data = sf::st_transform(poly2, 4326), color = "red") %>%
    layer_extras(cols = c("black", "red"), nms = c(poly1_nm, poly2_nm))

  return(out)
}


#' Make future ranges map
#'
#' Used for creating the future ranges map
#'
#' @param poly1 Range polygon
#' @param poly2 Assement area polygon
#' @param rast Future ranges polygon
#' @param max_cell Maximum cells in plotted raster
#'
#' @returns Leaflet map
#' @noRd
#'
#' @examples
#' d <- test_data()
#'
#' r <- d$rng_chg_rast
#' names(r) <- c("RCP_45", "RCP 85")
#'
#' map_range(poly1 = d$rng_poly, poly2 = d$assess_poly,
#'           rast = r, rng_chg_mat = d$rng_chg_mat)

map_range <- function(poly1, poly2, rast, rng_chg_mat, max_cell = 5000000) {

  rast <- terra::classify(rast, rcl = rng_chg_mat, right = NA)
  rast_nm <- "hs_rast"

  pal = c("grey", "#FF0000", "#FFC125", "#008000")
  brks = 0:3
  rast_lbl <- data.frame(label = c("Not suitable", "Lost", "Maintained", "Gained"),
                         value = c(0, 1, 2, 3))

  rast_vals <- terra::unique(rast, incomparables = TRUE) %>%
    unlist() %>%
    unique()
  rast_lbl <- bind_cols(rast_lbl, pal = pal) %>%
    filter(.data$value %in% rast_vals)
  pal <- rast_lbl$pal
  col_tbl <- data.frame(value = rast_lbl$value, col = pal)
  for(l in 1:terra::nlyr(rast)){
    terra::coltab(rast, layer = l) <- col_tbl
  }
  rast_lbl <- pull(rast_lbl, .data$label)

  # Names
  rast_nm <- pretty_names(rast_nm)
  poly1_nm <- pretty_names("range_poly")
  poly2_nm <- pretty_names("assess_poly")

  rast <- prep_raster_map(rast, rast_nm, max_cell)
  if(terra::nlyr(rast) > 1) rast_grp <- names(rast) else rast_grp <- character(0)

  # Base Map
  out <- layer_base() %>%
    layer_raster(rast, rast_nm, rast_lbl, pal, rast_grp) %>%
    leaflet::addPolylines(data = sf::st_transform(poly1, 4326), color = "black") %>%
    leaflet::addPolylines(data = sf::st_transform(poly2, 4326), color = "red") %>%
    layer_extras(c("black", "red"), nms = c(poly1_nm, poly2_nm), rast_grp)

  return(out)
}

#' Make protected areas map
#'
#' Used for creating the protected areas map
#'
#' @param poly1 Assessment area polygon
#' @param poly2 Protected areas polygon
#' @param rast Future ranges polygon
#' @param max_cell Maximum cells in plotted raster
#'
#' @returns Leaflet map
#' @noRd
#'
#' @examples
#' d <- test_data()
#'
#' r <- terra::classify(d$rng_chg_rast, rcl = d$rng_chg_mat, right = NA)
#' map_protected(poly1 = d$assess_poly, poly2 = d$protected_poly,
#'               rast = r)

map_protected <- function(poly1, poly2, rast, max_cell = 5000000) {

  rast_nm <- "hs_rast"
  rast <- terra::subst(rast, c(0, 1), NA)
  rast <- terra::subst(rast, c(2, 3), 1)
  rast_lbl <- c("Maintained or Gained")

  # Names
  rast_nm <- pretty_names(rast_nm)
  poly1_nm <- pretty_names("assess_poly")
  poly2_nm <- pretty_names("protected_poly")

  rast <- prep_raster_map(rast, rast_nm, max_cell)
  if(terra::nlyr(rast) > 1) rast_grp <- names(rast) else rast_grp <- character(0)

  # Base Map
  # Layer order follows:
  # - https://github.com/rstudio/leaflet/pull/549
  # - https://github.com/rstudio/leaflet/issues/427

  out <- layer_base() %>%
    leaflet::addMapPane("range", zIndex = 420) %>%
    leaflet::addMapPane("protected_areas", zIndex = 410) %>%
    leaflet::addPolylines(data = sf::st_transform(poly1, 4326), color = "black") %>%
    leaflet::addPolygons(data = sf::st_transform(poly2, 4326), color = NA,
                         fillColor = "darkgreen", fillOpacity = 0.8) %>%
    layer_extras(c("black", "darkgreen"), nms = c(poly1_nm, poly2_nm), rast_grp) %>%
    layer_raster(rast, rast_nm, rast_lbl, "#FFC125", rast_grp, opacity = 0.5,
                 options = leaflet::gridOptions(pane = "range"))

  return(out)
}

pretty_names <- function(nm) {
  # Name of input data layers for mapping
  rast_nms <- list(`Temperature exposure class` = "mat",
                   `Historical precipitation (mm)` = "map",
                   `Moisture class` = "cmd",
                   `Climate change exposure index` = "ccei",
                   `Historical thermal niche` = "htn",
                   `Modeled range change` = "hs_rast",
                   `Protected areas` = "protected_rast")

  poly_nms <- list(`Current range` = "range_poly",
                   `Assessment area`= "assess_poly",
                   `Non-breeding range` = "nonbreed_poly",
                   `Physiological thermal niche` = "ptn",
                   `Protected areas` = "protected_poly")

  all_nms <- c(rast_nms, poly_nms)

  if(nm %in% all_nms) nm <- names(all_nms)[all_nms == nm]
  nm
}

layer_base <- function() {
  leaflet::leaflet() %>%
    # Tiles first to go under rasters
    leaflet::addProviderTiles(
      leaflet::providers$OpenStreetMap, group = "OpenStreetMap") %>%
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.Positron, group = "CartoDB")
}


layer_extras <- function(out, cols, nms, rast_grp = character(0)) {
  out %>%
    leaflet::addLegend(colors = cols, labels = nms, opacity = 1) %>%
    leaflet::addLayersControl(
      baseGroups = c("CartoDB", "OpenStreetMap"),
      overlayGroups = rast_grp,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
}

layer_raster <- function(out, rast, rast_nm, rast_lbl, pal, rast_grp, rng_val = NULL,
                         opacity = 1, options = leaflet::gridOptions()) {
  for(l in 1:terra::nlyr(rast)){
    out <- leaflet::addRasterImage(out, x = rast[[l]], method = "ngb",
                                   colors = pal,
                                   group = rast_grp[l], opacity = opacity,
                                   options = options)
  }

  if(is.character(pal)) { # If character palette (categorical)
    out <- leaflet::addLegend(out, colors = pal, labels = rast_lbl,
                              title = rast_nm, opacity = 1)
  } else { # If palette function (continuous)
    out <- leaflet::addLegend(out, pal = pal, values = rng_val[1]:rng_val[2],
                              title = rast_nm, opacity = 1)
  }
  out
}
