
#' Run the spatial analysis functions to get inputs to index calculator
#'
#' @param species_nm
#' @param scale_nm
#' @param range_poly
#' @param non_breed_poly
#' @param scale_poly
#' @param hs_rast
#' @param clim_vars_lst
#'
#' @return
#' @export
#'
#' @examples
run_spatial <- function(range_poly, scale_poly, clim_vars_lst,
                        non_breed_poly = NULL, ptn_poly = NULL,
                        hs_rast = NULL){
  message("performing spatial analysis")

  # Check polygon inputs have only one feature and if not union
  range_poly <- check_polys(range_poly)
  scale_poly <- check_polys(scale_poly)
  non_breed_poly <- check_polys(non_breed_poly)
  ptn_poly <- check_polys(ptn_poly)

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
    htn_classes <- calc_prop_raster(clim_vars_lst$htn, range_poly, "HTN",
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
    range_MAP <- calc_min_max_raster(clim_vars_lst$map, range_poly, "MAP")
  }


  # Section D - Modelled Response to Climate Change #====
  if(is.null(hs_rast)){
    mod_resp_CC <- rep(NA_real_, 3) %>% as.list() %>% as.data.frame() %>%
      purrr::set_names(c("perc_lost", "perc_gain", "perc_maint"))

  } else {

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

  out <- bind_cols(mat_classes, cmd_classes, ccei_classes, not_overlap, htn_classes,
                   ptn_perc, range_MAP, mod_resp_CC, range_size)
  return(out)
}


# helper function to check input polys
check_polys <- function(poly){
  if(is.null(poly)){
    return(poly)
  }
  if(!is(poly, "sf")){
    poly <- sf::st_as_sf(poly)
  }
  if(nrow(poly) > 1){
    poly <- sf::st_union(poly) %>% sf::st_as_sf()
  }
  return(poly)
}

