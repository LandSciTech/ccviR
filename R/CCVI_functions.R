# Build functions needed for CCVI GIS analysis

# Get the proportion of each class in a raster that overlaps a polygon
calc_prop_raster <- function(rast, poly, var_name, scope, val_range = 1:6,
                             eer_pkg = requireNamespace("exactextractr",
                                                        quietly = TRUE)){
  if(eer_pkg){
    out <- exactextractr::exact_extract(rast, poly, progress = FALSE)
    out <- out[[1]] %>%
      filter(!is.na(value)) %>%
      mutate(value = factor(value, levels = val_range)) %>%
      group_by(value, .drop = FALSE) %>%
      summarise(sum = sum(coverage_fraction)) %>%
      transmute(value, prop = sum/sum(sum) * 100 %>% round(3))

  } else {
    message("install package exactextractr for much faster execution")
    out <- raster::extract(rast, poly,  df = TRUE)
    out <- prop.table(table(out[,2]))
    out <- round(100 * out, 3)
    out <- as.data.frame(out) %>% set_names("value", "prop")
  }

  out <- pivot_wider(out, names_from = "value", values_from = "prop",
                     names_prefix = paste0(var_name, "_"))
}

# get the proportion of poly1 that overlaps poly2
calc_overlap_poly <- function(poly1, poly2, var_name){
  int1_2 <- st_intersection(poly1, poly2)

  if(nrow(int1_2) == 0){
   out <- tibble(x = 0) %>% set_names(var_name)
   return(out)
  } else {
    int1_2 <- st_area(int1_2) %>% units::drop_units()
    area1 <- st_area(poly1) %>% units::drop_units()
    prop_area <- int1_2/area1 * 100
    out <- tibble(x = prop_area) %>% set_names(var_name)
    return(out)
  }
}

# Get the min and max of raster that overlaps a polygon
calc_min_max_raster <- function(rast, poly, var_name,
                                eer_pkg = requireNamespace("exactextractr",
                                                           quietly = TRUE)){
  if(eer_pkg){
    out <- exactextractr::exact_extract(rast, poly, progress = FALSE)
    out_min <- out[[1]] %>% pull(value) %>% min(na.rm = TRUE)
    out_max <- out[[1]] %>% pull(value) %>% max(na.rm = TRUE)
  } else {
    out <- raster::extract(rast, poly,  df = TRUE)
    out_min <- min(out[,2], na.rm = TRUE)
    out_max <- max(out[,2], na.rm = TRUE)
  }

  out <- tibble(max = out_max, min = out_min) %>%
    set_names(paste0(var_name, "_max"), paste0(var_name, "_min"))

}

# Get the proportion of habitat gained and lost in range
calc_gain_loss <- function(rast, poly,
                           eer_pkg){
  if(eer_pkg){
    out <- exactextractr::exact_extract(rast, poly, progress = FALSE)
    out <- out[[1]]

  } else {
    out <- raster::extract(rast, poly,  df = TRUE) %>%
      set_names("ID", "value")

    out <- out %>% mutate(coverage_fraction = 1)
  }

  out <- out %>% group_by(value) %>%
    summarise(coverage_fraction = sum(coverage_fraction)) %>%
    filter(!is.na(value), value > 0) %>%
    mutate(gain_loss = case_when(value == 1 ~ "lost",
                                 value > 1 & value < 7 ~ "maint",
                                 value == 7 ~ "gain") %>% factor(levels = c("gain", "lost", "maint"))) %>%
    group_by(gain_loss, .drop = FALSE) %>%
    summarise(coverage_fraction = sum(coverage_fraction)) %>%
    pivot_wider(names_from = gain_loss, values_from = coverage_fraction) %>%
    transmute(perc_lost = (lost/(lost + maint) * 100) %>% round(3),
              perc_gain = (gain/(lost + maint) * 100) %>% round(3),
              perc_maint = (maint/(lost+maint) * 100) %>% round(3))

  return(out)
}

# get the proportion of a polygon that does not overlap non-NA values of raster
perc_not_overlap <- function(rast, poly, var_name){
  # # First check if poly is 100% inside rast extent and if so assume full overlap
  # rast_bbox <- st_bbox(rast) %>% st_as_sfc()
  # contained <- st_contains(rast_bbox, poly, sparse = FALSE)
  #
  # if(contained[1,1]){
  #   return(tibble(x = 100) %>% set_names(var_name))
  # }
  # rast_poly <- spex::polygonize(rast)
  # rast_poly <- st_union(rast_poly)
  # dif <- st_difference(non_breed_poly, rast_poly)
  #
  # dif_area <- st_area(dif)
  # poly_area <- st_area(poly)
  # prop_area <- dif_area/poly_area * 100
  # out <- tibble(x = prop_area) %>% set_names(var_name)
  # return(out)
  # # user  system elapsed
  # # 151.00    2.00  153.25

  # Another option using mask will be less accurate near the poles but is much
  # faster
  r_crop <- crop(rast, poly)
  r_mask <- mask(r_crop, poly, updatevalue = NA, updateNA= TRUE)
  cells_overlap <- freq(r_mask, useNA = "no")[,2] %>% sum()
  area_cell <- area(r_mask, na.rm = TRUE) %>% cellStats("mean")

  area_overlap <- cells_overlap * area_cell *1000000

  poly_area <- st_area(poly) %>% units::drop_units()

  prop_area <- (poly_area - area_overlap)/poly_area * 100

  out <- tibble(x = prop_area) %>% set_names(var_name)
  return(out)

}

# run the above functions to get a CCVI for 1 scale and species
run_CCVI_funs <- function(species_nm, scale_nm, range_poly, non_breed_poly,
                           scale_poly, hs_rast, clim_vars_lst, eer_pkg){

  # Section A - Exposure to Local Climate Change: #====

  # Temperature
  mat_classes <- calc_prop_raster(clim_vars_lst$mat, range_poly, "MAT", eer_pkg)

  # Moisture
  cmd_classes <- calc_prop_raster(clim_vars_lst$cmd, range_poly, "CMD", eer_pkg)

  # Migratory Exposure
  if(st_is_empty(non_breed_poly)){
    ccei_classes <- rep(NA_real_, 4) %>% as.list() %>% as.data.frame() %>%
      set_names(paste0("CCEI_", 1:4))

    not_overlap <- data.frame(perc_non_breed_not_over_ccei = NA_real_)
  } else {
    non_breed_poly <-st_crop(non_breed_poly, c(xmin = -180, ymin = -85, xmax = -20, ymax = 180))
    ccei_classes <- calc_prop_raster(clim_vars_lst$ccei, non_breed_poly, "CCEI",
                                     val_range = 1:4,
                                     eer_pkg)

    not_overlap <- perc_not_overlap(clim_vars_lst$ccei, non_breed_poly,
                                    "perc_non_breed_not_over_ccei")
  }

  # Section C - Sensitivity and Adaptive Capacity: #====

  # Historical Thermal niche
  htn_classes <- calc_prop_raster(clim_vars_lst$htn, range_poly, "HTN",
                                  val_range = 1:4, eer_pkg)

  # Physiological Thermal niche
  ptn_perc <- calc_overlap_poly(range_poly %>%
                                  st_transform(st_crs(clim_vars_lst$tundra)),
                                clim_vars_lst$tundra, "PTN")

  # Historical Hydrological niche
  range_MAP <- calc_min_max_raster(clim_vars_lst$map, range_poly, "MAP",
                                   eer_pkg)

  # Section D - Modelled Response to Climate Change #====
  if(nrow(hs_rast)== 1){
    mod_resp_CC <- rep(NA_real_, 3) %>% as.list() %>% as.data.frame() %>%
      set_names(c("perc_lost", "perc_gain", "perc_maint"))

  } else {

  mod_resp_CC <- calc_gain_loss(hs_rast, scale_poly, eer_pkg)

  }

  # Range size
  range_size <- tibble(range_size = st_area(range_poly) %>% units::drop_units())

  # % Range in Canada (don't do for US range poly)
  if(scale_nm == "NA"){
    range_CAN <- calc_overlap_poly(range_poly, clim_vars_lst$can,
                                   "perc_overlap_CAN")
  } else {
    range_CAN <- tibble(perc_overlap_CAN = NA_real_)
  }

  outs <- lst(mat_classes, cmd_classes, ccei_classes, not_overlap, htn_classes,
               ptn_perc, range_MAP, mod_resp_CC, range_size, range_CAN)

  too_long <- map_lgl(outs, ~nrow(.x) > 1)

  if(any(too_long)){
    stop("the " , paste0(names(which(too_long)), sep = " "),
         " variables have multiple rows. Check polygon inputs")
  }

  out <- tibble(species = species_nm, scale = scale_nm) %>%
    bind_cols(mat_classes, cmd_classes, ccei_classes, not_overlap, htn_classes,
              ptn_perc, range_MAP, mod_resp_CC, range_size, range_CAN)
}

# load the files needed for the CCVI then run it.
# Assumes file structure with
# root_path/
#     species_files/
#       Folder_per_species/
#           NA/
#           CAN/
#           USA/
#           files that apply to all scales
#     clim_files/
#           NA/
#           CAN/
#           USA/
#           files that apply to all scales
#     scale_files/
run_CCVI_calcs <- function(species_nm, scale_nm, root_pth, force_crs = TRUE,
                           eer_pkg = requireNamespace("exactextractr",
                                                      quietly = TRUE)){
  message(paste0("running ", species_nm, " at scale: ", scale_nm))



  range_poly <- list.files(paste0(root_pth, "/species_files/", species_nm, "/",
                                  scale_nm),
                           pattern = paste0(species_nm, ".*shp$"),
                           full.names = TRUE) %>%
    st_read(agr = "constant", quiet = TRUE)

  if(nrow(range_poly) > 1){
    range_poly <- st_union(range_poly) %>% st_as_sf()
  }


  scale_poly <- list.files(paste0(root_pth, "/scale_files/"),
                           pattern = paste0(scale_nm, ".*shp$"),
                           full.names = TRUE) %>%
    st_read(agr = "constant", quiet = TRUE)

  if(nrow(scale_poly) > 1){
    scale_poly <- st_union(scale_poly) %>% st_as_sf()
  }

  non_breed_path <- list.files(paste0(root_pth, "/species_files/", species_nm),
                               pattern = "non_breed.*shp$", full.names = TRUE)

  if(length(non_breed_path) == 0){
    message("no non_breeding range file detected, assuming resident")
    non_breed_poly <- st_sf(x = NA_real_, geometry = st_sfc(NA),
                            crs = st_crs(range_poly))
  } else {
    non_breed_poly <- non_breed_path %>%
      st_read(agr = "constant", quiet = TRUE)

    if(nrow(non_breed_poly) > 1){
      non_breed_poly <- st_union(non_breed_poly) %>% st_as_sf()
    }
  }

  clim_vars <- list(
    mat = list.files(paste0(root_pth, "/clim_files/", scale_nm),
                     pattern = "MAT.*tif$",
                     full.names = TRUE) %>% raster(),

    map = list.files(paste0(root_pth, "/clim_files/"),
                     pattern = "MAP.*tif$",
                     full.names = TRUE) %>% raster(),

    tundra = list.files(paste0(root_pth, "/clim_files/"),
                        pattern = "tundra.*shp$",
                        full.names = TRUE) %>%
      st_read(agr = "constant", quiet = TRUE),

    cmd = list.files(paste0(root_pth, "/clim_files/", scale_nm),
                     pattern = "CMD.*tif$",
                     full.names = TRUE) %>% raster(),

    ccei = list.files(paste0(root_pth, "/clim_files/"),
                      pattern = "ccei.*tif$",
                      full.names = TRUE) %>% raster(),

    htn = list.files(paste0(root_pth, "/clim_files/"),
                     pattern = "MWMT.*tif$",
                     full.names = TRUE) %>% raster(),

    can = list.files(paste0(root_pth, "/scale_files/"),
                     pattern = paste0("CAN", ".*shp$"),
                     full.names = TRUE) %>%
      st_read(agr = "constant", quiet = TRUE)
  )

  if(nrow(clim_vars$tundra) > 1){
    clim_vars$tundra <- st_union(clim_vars$tundra) %>% st_as_sf()
  }

  hs_rast_path <- list.files(paste0(root_pth, "/species_files/", species_nm),
                             pattern = "classifiedchange.*tif", full.names = TRUE)

    if(length(hs_rast_path) == 0){
      message("no hs model")
      hs_rast <- raster(matrix(NA_real_),
                      crs = crs(clim_vars$mat))
    } else {
      hs_rast <- hs_rast_path %>%
        raster()
    }
   # check for consistent crs
  ref_crs <- st_crs(clim_vars$mat)

  # tundra is intentionally different because longlat doesn't work as well at
  # poles
  check_crs_clim <- clim_vars[c(1:2, 4:7)] %>%
    map_lgl(~st_crs(.x) == ref_crs)

  if(!all(check_crs_clim)){
    wch <- which(check_crs_clim == F)
    stop("the crs of ", names(wch), " does not match the clim_vars crs",
         call. = FALSE)
  }

  if(st_is_longlat(clim_vars$tundra)){
    stop("the tundra shapefile does not have a projected crs ",
         "which could cause issues near the poles",
         call. = FALSE)
  }

  # check that the crs of the species files matches the clim_vars
  check_crs_sp <- lst(range_poly, non_breed_poly, scale_poly, hs_rast) %>%
    map_lgl(~st_crs(.x) == ref_crs)

  if(!all(check_crs_sp)){
    wch <- which(check_crs_sp == F)
    if(force_crs){
      stop("the crs of ", names(wch), " does not match the clim_vars crs",
         call. = FALSE)
    } else {
      warning("the crs of ", names(wch), " does not match the clim_vars crs",
           call. = FALSE)
    }
  }

  run_CCVI_funs(species_nm, scale_nm, range_poly, non_breed_poly, scale_poly,
                hs_rast, clim_vars, eer_pkg = eer_pkg)

}

# project crs using gdalUtils::gdalwarp

wrap_gdalwarp <- function(rast, ref_crs, out_path, overwrite = FALSE, ...){
  if(!requireNamespace("gdalUtils", quietly = TRUE)){
    stop("package gdalUtils is required for this function. Call" ,
         "install.packages(\"gdalUtils\") to install it.")
  }
  input <- raster(rast)
  src_crs <- input %>% crs() %>% .@projargs
  gdalUtils::gdalwarp(rast, out_path, src_crs, ref_crs,
                      overwrite = overwrite, ...)
}
