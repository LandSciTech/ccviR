
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
#' @param hs_rast Optional. A SpatRaster object with results from a model of the
#'   change in the species' range caused by climate change. To supply different
#'   results for each scenario use a raster with multiple layers and ensure that the order of
#'   the layers matches the order of \code{scenario_names}.
#' @param hs_rcl a matrix used to classify \code{hs_rast} into 0: not suitable, 1:
#'   lost, 2: maintained, 3: gained. See \code{\link[terra]{classify}} for
#'   details on the matrix format.
#' @param protected_poly Optional. A sf polygon object with protected areas.
#' @param gain_mod a number between 0 and 1 that can be used to down-weight gains
#'   in the modeled range change under climate change
#' @param scenario_names character vector with names that identify multiple
#'   future climate scenarios.
#'
#' @inheritParams common_docs
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
#'   hs_rast = terra::rast(c(file.path(base_pth, "rng_chg_45.tif"),
#'                           file.path(base_pth, "rng_chg_85.tif"))),
#'   hs_rcl = matrix(c(-1, 0, 1, 1, 2, 3), ncol = 2),
#'   scenario_names = scn_nms
#' )
#'
#' # With only one range change scenario (suboptimal)
#'
#' spat_res <- analyze_spatial(
#'   range_poly = sf::read_sf(file.path(base_pth, "rng_poly.shp"), agr = "constant"),
#'   scale_poly = sf::read_sf(file.path(base_pth, "assess_poly.shp"), agr = "constant"),
#'   clim_vars_lst = clim_vars,
#'   hs_rast = terra::rast(file.path(base_pth, "rng_chg_45.tif")),
#'   hs_rcl = matrix(c(-1, 0, 1, 1, 2, 3), ncol = 2),
#'   scenario_names = scn_nms
#' )
#'
#' \dontrun{
#' # With protected areas
#' spat_res <- analyze_spatial(
#'   range_poly = sf::read_sf(file.path(base_pth, "rng_poly.shp"), agr = "constant"),
#'   scale_poly = sf::read_sf(file.path(base_pth, "assess_poly.shp"), agr = "constant"),
#'   protected_poly = sf::read_sf("misc/protected_areas/pa_north_america.gpkg"),
#'   clim_vars_lst = clim_vars,
#'   hs_rast = terra::rast(c(file.path(base_pth, "rng_chg_45.tif"),
#'                           file.path(base_pth, "rng_chg_85.tif"))),
#'   hs_rcl = matrix(c(-1, 0, 1, 1, 2, 3), ncol = 2),
#'   scenario_names = scn_nms
#' )
#' }
#'
#' \dontrun{
#' # With CCEI (using `non_breed` spatial example)
#' clim_vars <- get_clim_vars("misc/climate/processed", scenario_names = scn_nms)
#' spat_res <- analyze_spatial(
#'   range_poly = sf::read_sf(file.path(base_pth, "rng_poly.shp"), agr = "constant"),
#'   scale_poly = sf::read_sf(file.path(base_pth, "assess_poly.shp"), agr = "constant"),
#'   protected_poly = sf::read_sf("misc/protected_areas/pa_north_america.gpkg"),
#'   non_breed_poly = non_breed,
#'   clim_vars_lst = clim_vars,
#'   hs_rast = terra::rast(c(file.path(base_pth, "rng_chg_45.tif"),
#'                           file.path(base_pth, "rng_chg_85.tif"))),
#'   hs_rcl = matrix(c(-1, 0, 1, 1, 2, 3), ncol = 2),
#'   scenario_names = scn_nms
#' )
#' }

analyze_spatial <- function(
    range_poly, scale_poly, clim_vars_lst,
    non_breed_poly = NULL, ptn_poly = NULL,
    hs_rast = NULL, hs_rcl = NULL, protected_poly = NULL,
    gain_mod = 1, scenario_names = "Scenario 1", quiet = FALSE) {

  # Setup Progress messages
  n <- 6
  inform_prog("Checking files", quiet, n)

  # Check Climate variables
  check_clim_vars(clim_vars_lst)

  # Check Rasters
  clim_vars_lst <- check_rast(clim_vars_lst, quiet = quiet)
  hs_rast <- check_rast(hs_rast, var_name = "hs_rast", quiet = quiet)

  # Check Scenario names
  check_scn(clim_vars_lst, hs_rast, scenario_names)

  # Check polygon inputs have only one feature and if not union and crs
  crs_use <- sf::st_crs(clim_vars_lst$mat[[1]])

  clim_poly <- prep_polys(clim_vars_lst$clim_poly, crs_use, "Climate Data Extext", quiet)
  scale_poly <- prep_polys(scale_poly, crs_use, "Assessment Area", quiet)
  range_poly <- prep_polys(range_poly, crs_use, "Range", quiet)

  # Optional inputs
  ptn_poly <- prep_polys(ptn_poly, crs_use, "PTN", quiet)
  if(!is.null(hs_rast)) {
    protected_poly <- prep_polys(
      protected_poly, var_name = "Protected Areas",
      crs = st_crs(hs_rast),
      poly_clip = scale_poly, clip_name = "Assessment Area",
      quiet)
  }

  if(!is.null(non_breed_poly) & !is.null(clim_vars_lst$ccei[[1]])) {
    non_breed_poly <- prep_polys(non_breed_poly, sf::st_crs(clim_vars_lst$ccei[[1]]),
                                  "non-breeding range")
  } else if (!is.null(non_breed_poly)){
    non_breed_poly <- NULL
    message("non_breed_poly was supplied but ccei was not included in clim_vars_lst, ",
            "ignoring non_breed_poly")
  } else {
    non_breed_poly <- NULL
  }

  # Clip range to climate data polygon and to scale poly
  range_poly_clim <- clip_poly(range_poly, clim_poly, "Range", "Climate Data Extent", quiet)
  range_poly <- clip_poly(range_poly, scale_poly, "Range", "Assessment Area", quiet)

  # Section A - Exposure to Local Climate Change: #====
  inform_prog("Assessing local climate exposure", quiet, n)

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

    overlap <- ccei_classes$prop_non_breed_over_ccei

    if(any(overlap == 0)) {
      stop("The nonbreeding range polygon does not overlap at least one of ",
           "the CCEI raster(s)", call. = FALSE)
    }

    if(any(overlap < 0.4)) {
      warning(
        paste0(round(1 - overlap, 2) * 100, collapse = "% and "),
        "% of the nonbreeding range polygon does not overlap the CCEI raster(s).\n",
        "Migratory exposure index only reflects conditions in the area of overlap",
        call. = FALSE)
    }
  }

  # Section C - Sensitivity and Adaptive Capacity: #====
  inform_prog("Assessing thermal & hydrological niches", quiet, n)

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
  inform_prog("Assessing modelled response to climate change", quiet, n)

  if(is.null(hs_rast)) {
    mod_resp_CC <- rep(NA_real_, 2) %>% as.list() %>% as.data.frame() %>%
      purrr::set_names(c("range_change", "range_overlap"))

  } else {

    if(is.null(hs_rcl)){
      stop("hs_rcl is required when hs_rast is not NULL", call. = FALSE)
    }

    hs_rast <- terra::classify(hs_rast, rcl = hs_rcl, right = NA)

    if(any(terra::minmax(hs_rast)[2,] > 3)){
      stop("Reclassified range change raster values outside the expected range of 0-3 were found. ",
           "Check that all range change raster values are included in the reclassification matrix")
    }

    mod_resp_CC <- calc_gain_loss(hs_rast, scale_poly, gain_mod = gain_mod)
  }

  if(is.null(protected_poly) | is.null(hs_rast)) {
    protected <- data.frame(protected = NA_real_)
  } else {

    # Only keep actual range
    # set 0 (not suitable) and 1 (lost) to NA,
    # set 2 (maint) and 3 (gained) to 1
    range_future <- terra::subst(hs_rast, c(0, 1), NA) %>%
      terra::subst(c(2,3), 1)

    protected <- calc_prop_raster(
      range_future, protected_poly, var_name = "protected",
      val_range = 1) %>%
      select("protected" = "protected_1")
  }

  inform_prog("Finalizing outputs", quiet, n)

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
                                     range_MAP, mod_resp_CC, range_size,
                                     protected),
              range_poly_assess = range_poly,
              range_poly_clim = range_poly_clim,
              protected_poly_assess = protected_poly)

  return(out)
}

prep_polys <- function(poly, crs, var_name, quiet = FALSE, poly_clip = NULL, clip_name = NULL) {
  if(is.null(poly)) return(poly)

  inform_prog(paste("Preparing polygon", var_name), quiet)
  poly <- check_polys(poly, var_name, quiet)
  if(!is.null(poly_clip)) poly <- clip_poly(poly, poly_clip, var_name, clip_name, quiet)
  poly <- check_crs(poly, crs, var_name, quiet)
  poly <- valid_or_error(poly, var_name, quiet)

  return(poly)
}

clip_poly <- function(poly, poly_clip, var1, var2, quiet) {

  inform_prog(paste("Clipping", var1, "to", var2), quiet)

  clipped <- st_intersection(poly, sf::st_transform(poly_clip, sf::st_crs(poly))) %>%
    st_set_agr("constant")
  if(nrow(clipped) == 0 ||
     st_geometry_type(clipped) %in% c("LINESTRING", "MULTILINESTRING")) {
    stop("The ", var1, "polygon does not overlap with the ", var2, "polygon",
         call. = FALSE)
  }
  valid_or_error(clipped, var_name = paste(var1, var2, "intersection"))

  return(clipped)
}

