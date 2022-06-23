#' project crs using gdalUtils::gdalwarp
#'
#' @param rast Raster
#' @param ref_crs crs to transform to
#' @param out_path path to save output
#' @param overwrite should files at out_path be overwritten?
#' @param resamp_method resampling method
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
