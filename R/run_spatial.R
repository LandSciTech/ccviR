
#' Run the spatial analysis functions to get inputs to index calculator
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

  # Check polygon inputs have only one feature and if not union
  range_poly <- check_polys(range_poly)
  scale_poly <- check_polys(scale_poly)
  non_breed_poly <- check_polys(non_breed_poly)
  clim_vars_lst$ptn <- check_polys(clim_vars_lst$ptn)

  # Section A - Exposure to Local Climate Change: #====

  # Temperature
  mat_classes <- calc_prop_raster(clim_vars_lst$mat, range_poly, "MAT", eer_pkg)

  # Moisture
  cmd_classes <- calc_prop_raster(clim_vars_lst$cmd, range_poly, "CMD", eer_pkg)

  # Migratory Exposure
  if(is.null(non_breed_poly) || is.null(clim_vars_lst$ccei)){
    ccei_classes <- rep(NA_real_, 4) %>% as.list() %>% as.data.frame() %>%
      purrr::set_names(paste0("CCEI_", 1:4))

    not_overlap <- data.frame(perc_non_breed_not_over_ccei = NA_real_)
  } else {

    if(is(non_breed_poly, "sfc")){
      non_breed_poly <- sf::st_as_sf(non_breed_poly)
    }
    if(nrow(non_breed_poly) > 1){
      non_breed_poly <- sf::st_union(non_breed_poly) %>% sf::st_as_sf()
    }

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

# a funtion to trim NAs off rasters. There is a version raster::trim but it is
# really slow this version might have issues if the raster is really big
trim_ras <- function (ras, filename = "", overwrite = FALSE){
  NA_mat <- is.na(raster::as.matrix(ras))
  colnotNA <- which(colSums(NA_mat) != raster::nrow(ras))
  rownotNA <- which(rowSums(NA_mat) != raster::ncol(ras))
  ext <- raster::extent(ras, rownotNA[1], rownotNA[length(rownotNA)],
                        colnotNA[1], colnotNA[length(colnotNA)])
  out <- raster::crop(ras, ext, filename = filename, overwrite = overwrite)
}

#' Trim NAs from raster copied from raster package internal .memtrimlayer in
#' raster::trim. This is not memory safe but raster::trim takes over an hour
#' while this takes ~30s
#' @param x
#'
#' @param padding
#' @param values
#' @param filename
#' @param ...
#'
#' @export
trim_ras <- function(r, padding=0, values=NA, filename="", ...) {
  x <- raster::as.matrix(r)
  if (all(is.na(values))) {
    rows <- rowSums(is.na(x))
    cols <- colSums(is.na(x))
  } else {
    rows <- apply(x, 1, function(i) sum(i %in% values))
    cols <- apply(x, 2, function(i) sum(i %in% values))
  }
  rows <- which(rows != ncol(x))
  if (length(rows)==0) { 	stop("only NA values found") }
  cols <- which(cols != nrow(x))

  rows <- pmin(pmax(1, c(min(rows) - padding, max(rows + padding))), nrow(r))
  cols <- pmin(pmax(1, c(min(cols) - padding, max(cols + padding))), ncol(r))

  e <- raster::extent(r, rows[1], rows[2], cols[1], cols[2])
  raster::crop(r, e, filename=filename, ...)
}

#' @export
check_trim <- function(rast){
  do_trim <- sum(!is.na(rast[1:10,]))
  if(do_trim == 0){
    message("doing trim")
    rast <- trim_ras(rast)
  }
  return(rast)
}
