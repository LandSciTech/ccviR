check_clim_vars <- function(clim_vars_lst, hs_rast) {
  clim_nms_dif <- setdiff(names(clim_vars_lst),
                          c("mat", "cmd", "map", "ccei", "htn", "clim_poly"))

  if(length(clim_nms_dif) > 0){
    stop("clim_vars_lst has unexpected names: ", clim_nms_dif, call. = FALSE)
  }

  clim_nms_mis <- setdiff(c("mat", "cmd", "clim_poly"), names(clim_vars_lst))

  if(length(clim_nms_mis) > 0){
    stop("clim_vars_lst has missing required elements: ", clim_nms_mis,
         call. = FALSE)
  }
}

check_scn <- function(clim_vars_lst, hs_rast, scenario_names) {
  # Check scenario names match raster layers
  rast_lyrs <- purrr::keep(clim_vars_lst, ~inherits(.x, "SpatRaster")) %>%
    c(hs_rast = hs_rast) %>%
    purrr::compact() %>%
    purrr::map_dbl(terra::nlyr)

  if(!all(rast_lyrs %in% c(1, length(scenario_names)))){
    stop("rasters must have one layer or length(scenario_names) layers. ",
         "The rasters ",
         paste0(names(rast_lyrs)[which(!rast_lyrs %in% c(1, length(scenario_names)))],
                collapse = ", "),
         " do not have the correct number of layers.", call. = FALSE)
  }

}

#' Check and fix polygons
#'
#' @param poly Spaitial object. sf or convertible to sf.
#' @param var_name Character. Name of the variable for messaging
#'
#' @returns sf polygon
check_polys <- function(poly, var_name = "polygon", quiet = FALSE) {

  if(is.null(poly)) return(poly)
  if(!inherits(poly, "sf")) poly <- sf::st_as_sf(poly)

  poly <- check_zm(poly, var_name)

  validate(need(
    !is.na(st_crs(poly, quiet)),
    paste(var_name, " does not have a CRS.",
          " \nPlease load a file with a valid Coordinate Reference System")
  ))

  geo_type <- st_geometry_type(poly)
  if(any(!geo_type %in% c("POLYGON", "MULTIPOLYGON"))) {

    validate(need(
      any(geo_type %in% c("POLYGON", "MULTIPOLYGON")),
      paste0(var_name, " has geometry type ", unique(geo_type),
             " but only (MULTI)POLYGONs are accepted for this input.")
    ))

    poly <- st_collection_extract(poly, "POLYGON")
    if(!quiet) message("POINT or LINE geometries in ", var_name,
                       " were dropped.")
  }

  return(poly)
}

check_zm <- function(poly, var_name = "polygon") {
  a <- sf::st_geometry(poly) %>%
    attributes() %>%
    names()

  if(any(c("z_range", "m_range") %in% a)) {
    message("Removing Z and/or M dimensions from ", var_name)
    poly <- sf::st_zm(poly)
  }

  poly
}


check_rast <- function(ras, var_name, quiet = FALSE) {

  if(is.list(ras)) purrr::map2(ras, names(ras), check_rast)

  if(!inherits(ras, "SpatRaster")){
    if(inherits(ras, "Raster")){
      ras <- methods::as(ras, "SpatRaster")
    } else {
      return(ras)
    }
  }

  check_crs(ras, quiet = quiet)

  return(ras)
}



check_crs <- function(sp, crs_target = NULL, var_name = NULL, quiet = FALSE) {

  if(inherits(sp, c("SpatRaster", "RasterLayer", "Raster"))) {
    if(is.null(var_name)) var_name <- terra::sources(sp)
    if(is.na(terra::crs(sp))||terra::crs(sp) == ""){
      stop("The raster ", var_name, " does not have a CRS.",
           " \nPlease load a file with a valid Coordinate Reference System",
           call. = FALSE)
    }
  }

  if(!is.null(crs_target) && sf::st_crs(sp) != crs_target) {
    if(inherits(sp, "sf")) {
      inform_prog(paste("Transforming polygon", var_name), quiet)
      sp <- sf::st_transform(sp, crs_target)
    }
  }

  invisible(sp)
}
