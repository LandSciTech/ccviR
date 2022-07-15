# project crs using gdalUtils::gdalwarp

#' Title
#'
#' @param rast
#' @param ref_crs
#' @param out_path
#' @param overwrite
#' @param ...
#'
#' @noRd
wrap_gdalwarp <- function(rast, ref_crs, out_path, overwrite = FALSE,
                          resamp_method = "near", ...){
  # Not working
  # input <- raster(rast)
  # src_crs <- input %>% raster::crs() %>% .@projargs
  # sf::gdal_utils("warp", raster::filename(rast), out_path, quiet = FALSE,
  #                options = c(s_srs = src_crs, t_srs = ref_crs,
  #                            r = resamp_method, overwrite = overwrite,
  #                            ...))
  NULL
}
