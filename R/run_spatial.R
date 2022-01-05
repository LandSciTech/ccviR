
#' Run the spatial analysis
#'
#' Run the required spatial analysis to create the \code{spat_df} input for
#' \code{\link{calc_vulnerability}} and clip the range polygon to the
#' appropriate scales.
#'
#' The range polygon will be clipped to the area overlapping the
#' \code{scale_poly} and the also the the area overlapping the extent of the
#' climate data polygon. The range within the assessment area in used to
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
#' @param hs_rast Optional. A raster with results from a model of the change in
#'   habitat suitability caused by climate change.
#' @param hs_rcl a matrix used to classify \code{hs_rast} into 0: not suitable, 1:
#'   lost, 2: maintained, 3: gained. See \code{\link[raster]{reclassify}} for
#'   details on the matrix format
#'
#'
#' @return a list with elements: \code{spat_table} the results of the spatial
#'   analysis, \code{range_poly_assess} the range polygon clipped to the
#'   assessment area, and \code{range_poly_clim} the range polygon clipped to
#'   the extent of the climate data.
#'
#'   \code{spat_table} contains the following columns:
#'   \describe{
#'     \item{MAT_#}{The percentage of the species' range that is exposed to each class of change in mean annual temperature between the historical normal and predicted climate. Class 1 has the highest exposure and Class 6 the lowest}
#'     \item{CMD_#}{The percentage of the species' range that is exposed to each class of change in climate moisture deficit between the historical normal and predicted climate. Class 1 has the highest exposure and Class 6 the lowest}
#'     \item{CCEI_#}{The percentage of the species' non-breeding range that falls into each climate change exposure index class. Class 4 indicates high exposure while Class 1 indicates low exposure }
#'     \item{perc_non_breed_not_over_ccei}{The precentage of the non-breeding range that does not overlap with the CCEI raster data}
#'     \item{HTN_#}{The percentage of the species' range that is exposed to each class of variation between the historical coldest and warmest monts. Class 1 has the smallest variation and Class 4 is the largest}
#'     \item{PTN}{The percentage of the species' range that falls into cool or cold environments that may be lost or reduced in the assessment area as a result of climate change}
#'     \item{MAP_max/min}{The maximum and minimum historical mean annual precipitation in the species' range}
#'     \item{perc_lost/gain/maintained}{The percentage of the species' range that is predicted to be lost/gained/maintained under future climate conditions}
#'     \item{range_size}{The area of the species' range in m2}
#'     }
#'
#' @export
#'
#' @examples
#'
#' base_pth <- system.file("extData", package = "ccviR")
#'
#' clim_vars <- get_clim_vars(file.path(base_pth, "clim_files/processed"))
#'
#' run_spatial(
#'   range_poly = read_sf(file.path(base_pth, "rng_poly_high.shp")),
#'   scale_poly = read_sf(file.path(base_pth, "assess_poly.shp")),
#'   clim_vars_lst = clim_vars,
#'   hs_rast = raster::raster(file.path(base_pth, "HS_rast.tif")),
#'   hs_rcl = matrix(c(0:7, 0, 1, 2, 2 ,2, 2, 2, 3), ncol = 2)
#' )


run_spatial <- function(range_poly, scale_poly, clim_vars_lst,
                        non_breed_poly = NULL, ptn_poly = NULL,
                        hs_rast = NULL, hs_rcl = NULL){
  message("performing spatial analysis")

  # Check polygon inputs have only one feature and if not union and crs
  crs_use <- sf::st_crs(clim_vars_lst$mat)
  range_poly <- check_polys(range_poly, crs_use, "range polygon")
  scale_poly <- check_polys(scale_poly, crs_use, "assessment area polygon")
  non_breed_poly <- check_polys(non_breed_poly, crs_use, "non-breeding range polygon")
  ptn_poly <- check_polys(ptn_poly, crs_use, "PTN polygon")
  clim_poly <- check_polys(clim_vars_lst$clim_poly, crs_use, "climate data extext polygon")

  # Clip range to climate data polygon and to scale poly

  range_poly_clim <- st_intersection(range_poly, clim_poly)
  if(nrow(range_poly_clim) == 0){
    stop("The range polygon does not overlap with the climate data extent polygon.",
         call. = FALSE)
  }

  range_poly <- st_intersection(range_poly, scale_poly)
  if(nrow(range_poly) == 0){
    stop("The range polygon does not overlap with the assessment area polygon.",
         call. = FALSE)
  }

  # Section A - Exposure to Local Climate Change: #====

  # Temperature
  mat_classes <- calc_prop_raster(clim_vars_lst$mat, range_poly, "MAT")
  if(sum(mat_classes, na.rm = T) < 99){
    stop("The range polygon does not fully overlap the supplied temperature ",
         "raster.", call. = FALSE)
  }

  # Moisture
  cmd_classes <- calc_prop_raster(clim_vars_lst$cmd, range_poly, "CMD")
  if(sum(cmd_classes, na.rm = T) < 99){
    stop("The range polygon does not fully overlap the supplied moisture ",
         "raster.", call. = FALSE)
  }

  # Migratory Exposure
  if(is.null(non_breed_poly) || is.null(clim_vars_lst$ccei)){
    ccei_classes <- rep(NA_real_, 4) %>% as.list() %>% as.data.frame() %>%
      purrr::set_names(paste0("CCEI_", 1:4))

    not_overlap <- data.frame(perc_non_breed_not_over_ccei = NA_real_)
  } else {

    ccei_classes <- calc_prop_raster(clim_vars_lst$ccei, non_breed_poly, "CCEI",
                                     val_range = 1:4)

    not_overlap <- perc_not_overlap(clim_vars_lst$ccei, non_breed_poly,
                                    "perc_non_breed_not_over_ccei")
    if(not_overlap[1,1] > 60){
      warning(round(not_overlap[1,1], 2), "% of the nonbreeding range polygon does not",
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
  }

  # Historical Hydrological niche
  if(is.null(clim_vars_lst$map)){
    range_MAP <- data.frame(MAP_max = NA_real_, MAP_min = NA_real_)
  } else {
    range_MAP <- calc_min_max_raster(clim_vars_lst$map, range_poly_clim, "MAP")
  }


  # Section D - Modelled Response to Climate Change #====
  if(is.null(hs_rast)){
    mod_resp_CC <- rep(NA_real_, 3) %>% as.list() %>% as.data.frame() %>%
      purrr::set_names(c("perc_lost", "perc_gain", "perc_maint"))

  } else {

    hs_rast <- raster::reclassify(hs_rast, rcl = hs_rcl, right = NA)

    mod_resp_CC <- calc_gain_loss(hs_rast, scale_poly)
    if(sum(mod_resp_CC, na.rm = T) == 0){
      stop("The assessment area polygon does not overlap the supplied habitat suitability ",
           "raster.", call. = FALSE)
    }
  }

  # Range size
  range_size <- tibble(range_size = st_area(range_poly) %>% units::set_units(NULL))


  outs <- lst(mat_classes, cmd_classes, ccei_classes, not_overlap, htn_classes,
              ptn_perc, range_MAP, mod_resp_CC, range_size)

  too_long <- purrr::map_lgl(outs, ~nrow(.x) > 1)

  if(any(too_long)){
    stop("the " , paste0(names(which(too_long)), sep = " "),
         " variables have multiple rows. Check polygon inputs have only one feature")
  }

  out <- list(spat_table = bind_cols(mat_classes, cmd_classes, ccei_classes,
                                     not_overlap, htn_classes, ptn_perc,
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
  if(!is(poly, "sf")){
    poly <- sf::st_as_sf(poly)
  }

  if(!all(sf::st_is_valid(poly))){
    poly <- sf::st_make_valid(poly)

    if(!all(sf::st_is_valid(poly))){
      stop("The ", var_name, " is not valid. Check the polygon is ",
           "correct or provide a different version",
           call. = FALSE)
    }
  }
  if(nrow(poly) > 1){
    poly <- sf::st_union(poly) %>% sf::st_as_sf()
  }

  poly <- sf::st_transform(poly, rast_crs)
  return(poly)
}

