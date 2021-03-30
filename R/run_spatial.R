# run the GIS related functions to get inputs to index calculator for 1 scale
# and species
#' Title
#'
#' @param species_nm
#' @param scale_nm
#' @param range_poly
#' @param non_breed_poly
#' @param scale_poly
#' @param hs_rast
#' @param clim_vars_lst
#' @param eer_pkg
#'
#' @return
#' @export
#'
#' @examples
run_spatial <- function(range_poly, scale_poly, clim_vars_lst,
                        non_breed_poly = NULL,
                        hs_rast = NULL,
                        eer_pkg = requireNamespace("exactextractr",
                                                   quietly = TRUE)){
  message("performing spatial analysis")

  # Section A - Exposure to Local Climate Change: #====

  # Temperature
  mat_classes <- calc_prop_raster(clim_vars_lst$mat, range_poly, "MAT", eer_pkg)

  # Moisture
  cmd_classes <- calc_prop_raster(clim_vars_lst$cmd, range_poly, "CMD", eer_pkg)

  # Migratory Exposure
  if(is.null(non_breed_poly)){
    ccei_classes <- rep(NA_real_, 4) %>% as.list() %>% as.data.frame() %>%
      purrr::set_names(paste0("CCEI_", 1:4))

    not_overlap <- data.frame(perc_non_breed_not_over_ccei = NA_real_)
  } else {
    #non_breed_poly <- st_crop(non_breed_poly, c(xmin = -180, ymin = -85, xmax = -20, ymax = 180))
    ccei_classes <- calc_prop_raster(clim_vars_lst$ccei, non_breed_poly, "CCEI",
                                     val_range = 1:4,
                                     eer_pkg)

    not_overlap <- perc_not_overlap(clim_vars_lst$ccei, non_breed_poly,
                                    "perc_non_breed_not_over_ccei")
  }

  # Section C - Sensitivity and Adaptive Capacity: #====

  # Historical Thermal niche
  if(is.null(clim_vars_lst$htn)){
    htn_classes <- rep(NA_real_, 4) %>% as.list() %>% as.data.frame() %>%
      purrr::set_names(paste0("HTN_", 1:4))
  } else {
    htn_classes <- calc_prop_raster(clim_vars_lst$htn, range_poly, "HTN",
                                    val_range = 1:4, eer_pkg)
  }


  # Physiological Thermal niche
  if(is.null(clim_vars_lst$ptn)){
    ptn_perc <- data.frame(PTN = NA_real_)
  } else {
    if(st_crs(range_poly) != st_crs(clim_vars_lst$ptn)){
      clim_vars_lst$ptn <- clim_vars_lst$ptn %>%
        st_transform(st_crs(range_poly))
    }
    ptn_perc <- calc_overlap_poly(range_poly, clim_vars_lst$ptn, "PTN")
  }

  # Historical Hydrological niche
  if(is.null(clim_vars_lst$map)){
    range_MAP <- data.frame(MAP_max = NA_real_, MAP_min = NA_real_)
  } else {
    range_MAP <- calc_min_max_raster(clim_vars_lst$map, range_poly, "MAP",
                                     eer_pkg)
  }


  # Section D - Modelled Response to Climate Change #====
  if(is.null(hs_rast)){
    mod_resp_CC <- rep(NA_real_, 3) %>% as.list() %>% as.data.frame() %>%
      purrr::set_names(c("perc_lost", "perc_gain", "perc_maint"))

  } else {

    mod_resp_CC <- calc_gain_loss(hs_rast, scale_poly, eer_pkg)

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
}
