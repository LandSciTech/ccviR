# Functions to prepare data from raw to form needed for app


#' Prepare exposure data
#'
#' Prepare exposure data into classes based on delta and mean and
#' 1/2 standard deviation
#' @param rast_norm
#' @param rast_fut
#' @param file_nm
#' @param do_reproj
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
prep_exp <- function(rast_norm, rast_fut, file_nm, do_reproj = TRUE,
                     overwrite = FALSE){
  rast_delta <-  rast_norm - rast_fut

  if(do_reproj){
    ref_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

    # project raster to WGS84 and save to file_nm
    rast_delta <- wrap_gdalwarp(raster::filename(rast_delta), ref_crs,
                                overwrite = overwrite,
                                raster::rasterTmpFile(),
                                resamp_method = "bilinear",
                                output_Raster = TRUE)
  }

  mean_delta <- round(raster::cellStats(rast_delta, "mean"), 2)

  std_delta <- round(raster::cellStats(rast_delta, "sd")/2, 2)

  min_delta <- round(raster::cellStats(rast_delta, "min") -1, 2)

  max_delta <- round(raster::cellStats(rast_delta, "max") +1, 2)

  brs <- c(min_delta, mean_delta-2*std_delta, mean_delta-std_delta,
           mean_delta, mean_delta + std_delta, mean_delta + 2*std_delta,
           max_delta)

  rcl_tbl <- matrix(c(brs[1:6], brs[2:7], 1:6), ncol = 3)

  rast_reclass <- raster::reclassify(rast_delta, rcl_tbl, file_nm,
                                     overwrite = overwrite)

  return(rcl_tbl)

}

run_prep_data <- function(mat_norm, mat_fut, cmd_norm, cmd_fut, ccei,
                          map, mwmt, mcmt, in_folder, out_folder){

  # TODO: Format to use in and out folder. Change out names to CAPS. Figure out
  # if we should match Sarah O's intervals for reclassing

  prep_exp(mat_norm, mat_fut, "data/clim_files/SE_version/mat_reclass.tif")

  prep_exp(cmd_norm, cmd_fut, "data/clim_files/SE_version/cmd_reclass.tif")


  # Prepare other climate variables
  # CCEI
  ccei <- raster::raster("data/clim_files/raw/ccei.img")

  brs <- c(0, 4, 5, 7, 25)

  rcl_tbl <- matrix(c(brs[1:4], brs[2:5], 1:4), ncol = 3)
  ccei_reclass <- raster::reclassify(ccei, rcl_tbl)

  raster::writeRaster(ccei_reclass, "data/clim_files/SE_version/ccei_reclass.tif")

  # MAP
  ref_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  # project raster to WGS84 and save to file_nm
  wrap_gdalwarp("data/clim_files/raw/MAP.asc", ref_crs,
                "data/clim_files/SE_version/map.tif",
                resamp_method = "bilinear")

  # MWMT - MCMT

  mwmt <- raster::raster("data/clim_files/raw/MWMT.asc")
  mcmt <- raster::raster("data/clim_files/raw/MCMT.asc")

  dif_mt <- mwmt-mcmt

  ref_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  # project raster to WGS84
  dif_mt <- wrap_gdalwarp(raster::filename(dif_mt), ref_crs,
                          raster::rasterTmpFile(),
                          resamp_method = "bilinear",
                          output_Raster = TRUE)

  brs <- c(-1, 20.8, 26.3, 31.8, 50)

  rcl_tbl <- matrix(c(brs[1:4], brs[2:5], 1:4), ncol = 3)

  dif_mt_reclass <- raster::reclassify(dif_mt, rcl_tbl)

  raster::writeRaster(dif_mt_reclass, "data/clim_files/SE_version/mwmt_mcmt_reclass.tif")

}
