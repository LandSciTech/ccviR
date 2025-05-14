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
#' d <- test_data(protected = TRUE)
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
  out <- layer_base() %>%
    layer_raster(rast, rast_nm, rast_lbl, "#FFC125", rast_grp) %>%
    leaflet::addPolylines(data = sf::st_transform(poly1, 4326), color = "black") %>%
    leaflet::addPolygons(data = sf::st_transform(poly2, 4326), color = NA,
                         fillColor = "darkgreen", fillOpacity = 0.8) %>%
    layer_extras(c("black", "darkgreen"), nms = c(poly1_nm, poly2_nm), rast_grp)

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

layer_raster <- function(out, rast, rast_nm, rast_lbl, pal, rast_grp, rng_val = NULL) {
  for(l in 1:terra::nlyr(rast)){
    out <- leaflet::addRasterImage(out, x = rast[[l]], method = "ngb",
                                   colors = pal,
                                   group = rast_grp[l], opacity = 1)
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
