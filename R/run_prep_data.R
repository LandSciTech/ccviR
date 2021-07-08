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
prep_exp <- function(rast_norm, rast_fut, file_nm, reproject = TRUE,
                     overwrite = FALSE){
  rast_delta <-  rast_norm - rast_fut

  if(reproject){
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

#' Prepare data from raw to form needed for app
#'
#' Prepare data from raw to form needed for app including reclassifying and
#' reprojecting (optional)
#'
#' File names required in in_folder:
#' mat_norm: MAT.asc mean annual temperature for the historical normal period
#' mat_fut: MAT_2050.asc mean annual temperature for the future under climate change it can be any number eg 2050, 2100
#' cmd_norm: CMD.asc climate moisture deficit for the historical normal period
#' cmd_fut: CMD_2050.asc climate moisture deficit for the future under climate change it can be any number eg 2050, 2100
#' ccei: CCEI.img Climate Change Exposure Index from NatureServe website
#' map: MAP.asc mean annual precipitation for the historical normal period
#' mwmt: MWMT.asc mean warmest month temperature for the historical normal period
#' mcmt: MCMT.asc mean coldest month temperature for the historical normal period
#'
#' @param mat_norm,mat_fut,cmd_norm,cmd_fut,ccei,map,mwmt,mcmt filepaths to find
#'   data if in_folder is not given
#' @param in_folder filepath where files are stored. Files must be named
#'   according to the convention described in details
#' @param out_folder
#' @param reproject
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
run_prep_data <- function(mat_norm, mat_fut, cmd_norm, cmd_fut, ccei = NULL,
                          map = NULL, mwmt = NULL, mcmt = NULL, in_folder = NULL, out_folder,
                          reproject = TRUE, overwrite = FALSE){

  # TODO: Figure out if we should match Sarah O's intervals for reclassing.

  if(!is.null(in_folder)){
    mat_norm <- list.files(in_folder, pattern = "MAT....$", full.names = TRUE)

    mat_fut <- list.files(in_folder, pattern = "MAT_\\d.*", full.names = TRUE)

    cmd_norm <-list.files(in_folder, pattern = "CMD....$", full.names = TRUE)

    cmd_fut <- list.files(in_folder, pattern = "CMD_\\d.*", full.names = TRUE)

    if(any(lengths(list(mat_norm, mat_fut, cmd_norm, cmd_fut)) == 0)){
      stop("mat_norm, mat_fut, cmd_norm, cmd_fut must all be present in in_folder",
           call. = FALSE)
    }

    ccei <- list.files(in_folder, pattern = "CCEI.img$", full.names = TRUE)

    map <- list.files(in_folder, pattern = "MAP....$", full.names = TRUE)

    mwmt <- list.files(in_folder, pattern = "MWMT....$", full.names = TRUE)

    mcmt <- list.files(in_folder, pattern = "MCMT....$", full.names = TRUE)
  }

  mat_norm <- raster::raster(mat_norm)

  mat_fut <- raster::raster(mat_fut)

  cmd_norm <- raster::raster(cmd_norm)

  cmd_fut <- raster::raster(cmd_fut)

  if(!is.null(ccei) && length(ccei) > 0){
    ccei <- raster::raster(ccei)
  } else {
    ccei <- NULL
  }

  if(!is.null(map) && length(map) > 0){
    map_pth <- map
  } else {
    map_pth <- NULL
  }

  if(!is.null(mwmt) && length(mwmt) > 0){
    mwmt <- raster::raster(mwmt)
  } else {
    mwmt <- NULL
  }

  if(!is.null(mcmt) && length(mcmt) > 0){
    mcmt <- raster::raster(mcmt)
  } else {
    mcmt <- NULL
  }

  # check for crs
  purrr::map(purrr::compact(list(mat_norm, mat_fut, cmd_norm, cmd_fut, ccei,
                                 mwmt, mcmt)), check_crs)

  message("processing MAT")

  prep_exp(mat_norm, mat_fut, file.path(out_folder,"MAT_reclass.tif"),
           reproject = reproject, overwrite = overwrite)

  rm(mat_fut, mat_norm)

  message("processing CMD")

  prep_exp(cmd_norm, cmd_fut, file.path(out_folder,"CMD_reclass.tif"),
           reproject = reproject, overwrite = overwrite)

  rm(cmd_fut, cmd_norm)

  # Prepare other climate variables
  if(!is.null(ccei)){
    # CCEI
    message("processing CCEI")
    brs <- c(0, 4, 5, 7, 25)

    rcl_tbl <- matrix(c(brs[1:4], brs[2:5], 1:4), ncol = 3)
    ccei_reclass <- raster::reclassify(ccei, rcl_tbl)

    raster::writeRaster(ccei_reclass, file.path(out_folder, "CCEI_reclass.tif"),
                        overwrite = overwrite)

    rm(ccei_reclass, rcl_tbl, brs, ccei)
  }


  # MAP
  if(!is.null(map_pth)){
    message("processing MAP")

    if(reproject){
      ref_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

      # project raster to WGS84 and save to file_nm
      wrap_gdalwarp(map_pth, ref_crs,
                    file.path(out_folder, "MAP.tif"),
                    resamp_method = "bilinear")
    } else {
      map <- raster::raster(map_pth)
      check_crs(map)
      raster::writeRaster(map, file.path(out_folder, "MAP.tif"),
                          overwrite = overwrite)
      rm(map)
    }
  }

  # MWMT - MCMT
  if(!is.null(mwmt) && !is.null(mcmt)){
    message("processing MWMT and MCMT")
    dif_mt <- mwmt-mcmt

    rm(mwmt, mcmt)

    if(reproject){
      ref_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

      # project raster to WGS84
      dif_mt <- wrap_gdalwarp(raster::filename(dif_mt), ref_crs,
                              raster::rasterTmpFile(),
                              resamp_method = "bilinear",
                              output_Raster = TRUE)
    }

    brs <- c(-1, 20.8, 26.3, 31.8, 50)

    rcl_tbl <- matrix(c(brs[1:4], brs[2:5], 1:4), ncol = 3)

    dif_mt_reclass <- raster::reclassify(dif_mt, rcl_tbl)

    raster::writeRaster(dif_mt_reclass,
                        file.path(out_folder, "MWMT_MCMT_reclass.tif"),
                        overwrite = overwrite)
  }

}

check_crs <- function(rast){
  if(is.na(raster::crs(rast))){
    stop("the raster ", raster::filename(rast),
         " has no CRS. Please provide a file with CRS information",
         call. = FALSE)
  }
}
