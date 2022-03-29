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
  if(!requireNamespace("gdalUtils", quietly = TRUE)){
    stop("package gdalUtils is required for this function. Call" ,
         "install.packages(\"gdalUtils\") to install it.")
  }
  input <- raster(rast)
  src_crs <- input %>% raster::crs() %>% .@projargs
  gdalUtils::gdalwarp(rast, out_path, src_crs, ref_crs,
                      overwrite = overwrite,
                      r = resamp_method,
                      ...)
}