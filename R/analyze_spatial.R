
#' Run the spatial analysis
#'
#' Run the required spatial analysis to create the \code{spat_df} input for
#' \code{\link{calc_vulnerability}} and clip the range polygon to the
#' appropriate scales.
#'
#' The range polygon will be clipped to the area overlapping the
#' \code{scale_poly} and also to the area overlapping the extent of the
#' climate data polygon. The range within the assessment area is used to
#' calculate all results except the historical thermal and hydrological niches
#' for which the range within the extent of the climate data is used.
#'
#' @param range_poly an sf polygon object giving the species range.
#' @param scale_poly an sf polygon object giving the area of the assessment
#' @param clim_vars_lst a list of climate data, the result of
#'   \code{\link{get_clim_vars}}
#' @param non_breed_poly Optional. An sf polygon object giving the species range
#'   in the non-breeding season.
#' @param ptn_poly Optional. An sf polygon object giving the locations that are
#'   considered part of the physiological thermal niche (See NatureServe
#'   Guidelines for definition).
#' @param hs_rast Optional. A Raster* object with results from a model of the
#'   change in the species' range caused by climate change. To supply different
#'   results for each scenario use a RasterStack and ensure that the order of
#'   the layers matches the order of \code{scenario_names}.
#' @param hs_rcl a matrix used to classify \code{hs_rast} into 0: not suitable, 1:
#'   lost, 2: maintained, 3: gained. See \code{\link[raster]{reclassify}} for
#'   details on the matrix format.
#' @param gain_mod a number between 0 and 1 that can be used to down-weight gains
#'   in the modeled range change under climate change
#' @param scenario_names character vector with names that identify multiple
#'   future climate scenarios.
#'
#' @return a list with elements: \code{spat_table} the results of the spatial
#'   analysis, \code{range_poly_assess} the range polygon clipped to the
#'   assessment area, and \code{range_poly_clim} the range polygon clipped to
#'   the extent of the climate data.
#'
#'   \code{spat_table} contains the following columns:
#'   \describe{
#'     \item{scenario_name}{Name identifying the scenario}
#'     \item{MAT_#}{The percentage of the species' range that is exposed to each class of change in mean annual temperature between the historical normal and predicted climate. Class 1 has the highest exposure and Class 6 the lowest}
#'     \item{CMD_#}{The percentage of the species' range that is exposed to each class of change in climate moisture deficit between the historical normal and predicted climate. Class 1 has the highest exposure and Class 6 the lowest}
#'     \item{CCEI_#}{The percentage of the species' non-breeding range that falls into each climate change exposure index class. Class 4 indicates high exposure while Class 1 indicates low exposure }
#'     \item{perc_non_breed_not_over_ccei}{The precentage of the non-breeding range that does not overlap with the CCEI raster data}
#'     \item{HTN_#}{The percentage of the species' range that is exposed to each class of variation between the historical coldest and warmest monts. Class 1 has the smallest variation and Class 4 is the largest}
#'     \item{PTN}{The percentage of the species' range that falls into cool or cold environments that may be lost or reduced in the assessment area as a result of climate change}
#'     \item{MAP_max/min}{The maximum and minimum historical mean annual precipitation in the species' range}
#'     \item{range_change}{The projected decrease in range size as a percentage of current range size. Negative numbers indicate an increase in range size}
#'     \item{range_overlap}{The percentage of the current range that is projected to remain in the future range.}
#'     \item{range_size}{The area of the species' range in m2}
#'     }
#'
#' @export
#'
#' @examples
#'
#' base_pth <- system.file("extdata", package = "ccviR")
#'
#' # scenario names
#' scn_nms <- c("RCP 4.5", "RCP 8.5")
#'
#' clim_vars <- get_clim_vars(file.path(base_pth, "clim_files/processed"),
#'                            scenario_names = scn_nms)
#'
#' spat_res <- analyze_spatial(
#'   range_poly = sf::read_sf(file.path(base_pth, "rng_poly.shp"), agr = "constant"),
#'   scale_poly = sf::read_sf(file.path(base_pth, "assess_poly.shp"), agr = "constant"),
#'   clim_vars_lst = clim_vars,
#'   hs_rast = raster::stack(raster::raster(file.path(base_pth, "rng_chg_45.tif")),
#'                           raster::raster(file.path(base_pth, "rng_chg_85.tif"))),
#'   hs_rcl = matrix(c(0:7, 0, 1, 2, 2 ,2, 2, 2, 3), ncol = 2),
#'   scenario_names = scn_nms
#' )

analyze_spatial <- function(range_poly, scale_poly, clim_vars_lst,
                        non_breed_poly = NULL, ptn_poly = NULL,
                        hs_rast = NULL, hs_rcl = NULL, gain_mod = 1,
                        scenario_names = "Scenario 1"){
  message("performing spatial analysis")

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

  # Check scenario names match raster stacks
  rast_lyrs <- purrr::keep(clim_vars_lst, ~is(.x, "Raster")) %>%
    list(hs_rast = hs_rast) %>%
    purrr::list_flatten() %>%
    purrr::compact() %>%
    purrr::map_dbl(raster::nlayers)

  if(!all(rast_lyrs %in% c(1, length(scenario_names)))){
    stop("rasters must have one layer or length(scenario_names) layers. ",
         "The rasters ",
         paste0(names(rast_lyrs)[which(!rast_lyrs %in% c(1, length(scenario_names)))],
                collapse = ", "),
         " do not have the correct number of layers.", call. = FALSE)
  }

  # Check polygon inputs have only one feature and if not union and crs
  crs_use <- sf::st_crs(clim_vars_lst$mat[[1]])
  range_poly <- check_polys(range_poly, crs_use, "range polygon")
  scale_poly <- check_polys(scale_poly, crs_use, "assessment area polygon")
  non_breed_poly <- check_polys(non_breed_poly, sf::st_crs(clim_vars_lst$ccei[[1]]), "non-breeding range polygon")
  ptn_poly <- check_polys(ptn_poly, crs_use, "PTN polygon")
  clim_poly <- check_polys(clim_vars_lst$clim_poly, crs_use, "climate data extext polygon")

  # Clip range to climate data polygon and to scale poly

  range_poly_clim <- st_intersection(range_poly, clim_poly) %>%
    st_set_agr("constant")
  if(nrow(range_poly_clim) == 0){
    stop("The range polygon does not overlap with the climate data extent polygon.",
         call. = FALSE)
  }

  # sometimes intersection makes it invalid
  range_poly_clim <- valid_or_error(range_poly_clim, "range_poly clim_poly intersection")

  range_poly <- st_intersection(range_poly, scale_poly) %>% st_set_agr("constant")
  if(nrow(range_poly) == 0 ||
     st_geometry_type(range_poly) %in% c("LINESTRING", "MULTILINESTRING")){
    stop("The range polygon does not overlap with the assessment area polygon.",
         call. = FALSE)
  }

  # sometimes intersection makes it invalid
  range_poly <- valid_or_error(range_poly, "range_poly assessment area intersection")

  # Section A - Exposure to Local Climate Change: #====

  # Temperature
  mat_classes <- calc_prop_raster(clim_vars_lst$mat, range_poly, "MAT")

  # Moisture
  cmd_classes <- calc_prop_raster(clim_vars_lst$cmd, range_poly, "CMD")

  # Migratory Exposure
  if(is.null(non_breed_poly) || is.null(clim_vars_lst$ccei)){
    ccei_classes <- rep(NA_real_, 5) %>% as.list() %>% as.data.frame() %>%
      purrr::set_names(c(paste0("CCEI_", 1:4), "prop_non_breed_over_ccei"))
  } else {

    ccei_classes <- calc_prop_raster(clim_vars_lst$ccei, non_breed_poly, "CCEI",
                                     val_range = 1:4, check_overlap = 0,
                                     return_overlap_as = "prop_non_breed_over_ccei")

    overlap <- ccei_classes$prop_non_breed_over_ccei[1]

    if(overlap == 0){
      stop("The nonbreeding range polygon does not overlap the CCEI raster")
    }

    if(overlap < 0.4){
      warning(round(1-overlap, 2) *100, "% of the nonbreeding range polygon does not",
              " overlap the CCEI raster. Migratory exposure index only reflects ",
              "conditions in the area of overlap",
              call. = FALSE)
    }
  }

  # Section C - Sensitivity and Adaptive Capacity: #====

  # Historical Thermal niche
  if(is.null(clim_vars_lst$htn)){
    htn_classes <- rep(NA_real_, 4) %>% as.list() %>% as.data.frame() %>%
      purrr::set_names(paste0("HTN_", 1:4))
  } else {
    htn_classes <- calc_prop_raster(clim_vars_lst$htn, range_poly_clim, "HTN",
                                    val_range = 1:4)
  }


  # Physiological Thermal niche
  if(is.null(ptn_poly)){
    ptn_perc <- data.frame(PTN = NA_real_)
  } else {
    if(st_crs(range_poly) != st_crs(ptn_poly)){
      ptn_poly <- st_transform(ptn_poly, st_crs(range_poly))
    }

    ptn_perc <- calc_overlap_poly(range_poly, ptn_poly, "PTN")

    if(ptn_perc$PTN == 0){
      if(!st_intersects(scale_poly, ptn_poly, sparse = FALSE)[1,1]){
        stop("The phsiological thermal niche polygon does not overlap the assessment area",
             call. = FALSE)
      }
    }
  }

  # Historical Hydrological niche
  if(is.null(clim_vars_lst$map)){
    range_MAP <- data.frame(MAP_max = NA_real_, MAP_min = NA_real_)
  } else {
    range_MAP <- calc_min_max_raster(clim_vars_lst$map, range_poly_clim, "MAP")
  }


  # Section D - Modelled Response to Climate Change #====
  if(is.null(hs_rast)){
    mod_resp_CC <- rep(NA_real_, 2) %>% as.list() %>% as.data.frame() %>%
      purrr::set_names(c("range_change", "range_overlap"))

  } else {

    if(is.null(hs_rcl)){
      stop("hs_rcl is required when hs_rast is not NULL", call. = FALSE)
    }

    hs_rast <- raster::reclassify(hs_rast, rcl = hs_rcl, right = NA)

    if(any(raster::maxValue(hs_rast) > 3)){
      stop("Reclassified range change raster values outside the expected range of 0-3 were found. ",
           "Check that all range change raster values are included in the reclassification matrix")
    }

    mod_resp_CC <- calc_gain_loss(hs_rast, scale_poly, gain_mod = gain_mod)

  }

  # Range size
  range_size <- tibble(range_size = st_area(range_poly) %>% units::set_units(NULL))


  # outs <- lst(mat_classes, cmd_classes, ccei_classes, htn_classes,
  #             ptn_perc, range_MAP, mod_resp_CC, range_size)
  #
  # too_long <- purrr::map_lgl(outs, ~nrow(.x) > 1)
  #
  # if(any(too_long)){
  #   stop("the " , paste0(names(which(too_long)), sep = " "),
  #        " variables have multiple rows. Check polygon inputs have only one feature")
  # }

  scn_nm <- data.frame(scenario_name = scenario_names)

  out <- list(spat_table = bind_cols(scn_nm, mat_classes, cmd_classes, ccei_classes,
                                     htn_classes, ptn_perc,
                                     range_MAP, mod_resp_CC, range_size),
              range_poly_assess = range_poly,
              range_poly_clim = range_poly_clim)
  return(out)
}


# helper function to check input polys
check_polys <- function(poly, rast_crs, var_name){
  if(is.null(poly)){
    return(poly)
  }
  if(!inherits(poly, "sf")){
    poly <- sf::st_as_sf(poly)
  }

  if(is.na(st_crs(poly))){
    stop(var_name, " does not have a CRS.",
         " \nPlease load a file with a valid Coordinate Reference System",
         call. = FALSE)
  }

  poly <- sf::st_transform(poly, rast_crs)

  poly <- valid_or_error(poly, var_name)

  return(poly)
}


