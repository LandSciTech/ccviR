# load the files needed for the CCVI then run it for the process used by Sarah
# Ouimette 2020

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
#' Title
#'
#' @param species_nm
#' @param scale_nm
#' @param root_pth
#' @param force_crs
#' @param eer_pkg
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import sf
#' @importFrom raster raster crs
run_CCVI_calcs_SO <- function(species_nm, scale_nm, root_pth, force_crs = TRUE,
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
    purrr::map_lgl(~st_crs(.x) == ref_crs)

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
    purrr::map_lgl(~st_crs(.x) == ref_crs)

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


