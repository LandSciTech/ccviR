#' Prepare climate data
#'
#' Prepare data from raw to form needed for calculating the index. See the
#' NatureServe Guidelines for details on how the data is prepared.
#'
#' Definition of input data sets and file names required in in_folder:
#' \describe{
#'   \item{mat_norm:}{"MAT" mean annual temperature for the historical normal period}
#'   \item{mat_fut:}{"MAT_2050" mean annual temperature for the future under
#'   climate change it can be any number eg 2050, 2100}
#'   \item{cmd_norm:}{"CMD" climate moisture deficit for the historical normal period}
#'   \item{cmd_fut:}{"CMD_2050" climate moisture deficit for the future under
#'   climate change it can be any number eg 2050, 2100}
#'   \item{ccei:}{"CCEI" Climate Change Exposure Index from NatureServe website}
#'   \item{map:}{"MAP" mean annual precipitation for the historical normal period}
#'   \item{mwmt:}{"MWMT" mean warmest month temperature for the historical
#'   normal period}
#'   \item{mcmt:}{"MCMT" mean coldest month temperature for the historical
#'   normal period}
#'   \item{clim_poly:}{An optional shapefile with a polygon of the range of the
#'   climate data. It will be created from the climate data if it is missing
#'   but it is faster to provide it.}
#' }
#' Accepted raster file types are ".asc", ".tif", ".nc", ".grd" and ".img"

#' @param mat_norm,mat_fut,cmd_norm,cmd_fut,ccei,map,mwmt,mcmt,clim_poly
#'   file paths to find data if in_folder is not given
#' @param in_folder file path where files are stored. Files must be named
#'   according to the convention described in details
#' @param out_folder file path where the processed files will be saved
#' @param reproject should the data be re-projected to lat/long? Not recommended.
#' @param overwrite should existing files in out_folder be overwritten?
#' @param scenario_name a string identifying the climate change scenario that
#'   will be used as a suffix for the output files.
#' @param brks_mat,brks_cmd,brks_ccei a matrix containing breaks to use for
#'   classifying mat, cmd and ccei into 6, 6 and 4 classes, respectively. See
#'   \code{\link[raster]{reclassify}} for details on the matrix format. If NULL,
#'   the default, the breaks will be determined using the median and half the
#'   interquartile range
#'
#' @return Returns a list of matrices with the breaks used to classify mat, cmd
#'   and ccei. This list can be used to  Processed data is saved in \code{out_folder}
#'
#' @seealso \code{\link{get_clim_vars}} for loading the processed data.
#'
#' @export
#'
#' @examples
#' pth_in <- system.file("extData/clim_files/raw", package = "ccviR")
#'
#' pth_out <- system.file("extData/clim_files/processed", package = "ccviR")
#'
#' run_prep_data(in_folder = pth_in, out_folder = pth_out, overwrite = TRUE)
run_prep_data <- function(mat_norm, mat_fut, cmd_norm, cmd_fut, ccei = NULL,
                          map = NULL, mwmt = NULL, mcmt = NULL, clim_poly = NULL,
                          in_folder = NULL, out_folder,
                          reproject = FALSE, overwrite = FALSE,
                          scenario_name = "", brks_mat = NULL,
                          brks_cmd = NULL, brks_ccei = NULL){
  if(length(out_folder) == 0 || missing(out_folder)){
    stop("out_folder is missing with no default")
  }

  if(!dir.exists(out_folder)){
    stop("out_folder does not exist", call. = FALSE)
  }

  # TODO: Figure out if we should match Sarah O's intervals for reclassing.
  ext_accept <- c(".asc", ".tif", ".nc", ".grd", ".img")

  make_pat <- function(x, ext_accept){
    paste0(x, ext_accept, "$", collapse = "|")
  }

  if(!is.null(in_folder)){

    if(!dir.exists(in_folder)){
      stop("in_folder does not exist", call. = FALSE)
    }

    mat_norm <- list.files(in_folder,
                           pattern = make_pat("MAT", ext_accept),
                           full.names = TRUE)

    mat_fut <- list.files(in_folder,
                          pattern = make_pat("MAT_\\d.*", ext_accept),
                          full.names = TRUE)

    cmd_norm <-list.files(in_folder,
                          pattern = make_pat("CMD", ext_accept),
                          full.names = TRUE)

    cmd_fut <- list.files(in_folder,
                          pattern = make_pat("CMD_\\d.*", ext_accept),
                          full.names = TRUE)

    missing <- purrr::map_lgl(lst(mat_norm, mat_fut, cmd_norm, cmd_fut),
                               ~length(.x) == 0)
    if(any(missing)){
      stop("None of the files in ", in_folder,
           " matches the expected filename for ",
           paste0(names(missing)[which(missing)], sep = ", "), call. = FALSE)
    }

    ccei <- list.files(in_folder,
                       pattern = make_pat("CCEI", ext_accept),
                       full.names = TRUE)

    map <- list.files(in_folder,
                      pattern = make_pat("MAP", ext_accept),
                      full.names = TRUE)

    mwmt <- list.files(in_folder,
                       pattern = make_pat("MWMT", ext_accept),
                       full.names = TRUE)

    mcmt <- list.files(in_folder,
                       pattern = make_pat("MCMT", ext_accept),
                       full.names = TRUE)

    clim_poly <- list.files(in_folder,
                       pattern = make_pat("clim_poly", ".shp"),
                       full.names = TRUE)

    too_long <- purrr::map_lgl(lst(mat_norm, mat_fut, cmd_norm, cmd_fut, ccei,
                                   map, mwmt, mcmt, clim_poly),
                               ~length(.x) > 1)
    if(any(too_long)){
      stop("more than one file in ", in_folder,
           " matches the expected filename for ",
           paste0(names(too_long)[which(too_long)], sep = ", "), call. = FALSE)
    }
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

  if(!is.null(clim_poly) && length(clim_poly) > 0){
    clim_poly <- sf::read_sf(clim_poly, agr = "constant")
  } else {
    clim_poly <- NULL
  }

  # check for crs
  purrr::map(purrr::compact(list(mat_norm, mat_fut, cmd_norm, cmd_fut, ccei,
                                 mwmt, mcmt)), check_crs)

  message("processing MAT")

  brks_mat <- prep_exp(mat_norm, mat_fut,
                       file.path(out_folder, paste0("MAT_reclass", scenario_name, ".tif")),
                       reproject = reproject, overwrite = overwrite,
                       brs = brks_mat)

  rm(mat_fut, mat_norm)

  message("processing CMD")

  brks_cmd <- prep_exp(cmd_norm, cmd_fut,
                       file.path(out_folder, paste0("CMD_reclass", scenario_name, ".tif")),
                       reproject = reproject, overwrite = overwrite,
                       brs = brks_cmd)

  rm(cmd_fut, cmd_norm)

  # Prepare other climate variables
  if(!is.null(ccei)){
    # CCEI
    message("processing CCEI")
    if(is.null(brks_ccei)){
      brks_ccei <- c(0, 4, 5, 7, 25)

      rcl_tbl_ccei <- matrix(c(brks_ccei[1:4], brks_ccei[2:5], 1:4), ncol = 3)
    } else {
      rcl_tbl_ccei <- brks_ccei
    }

    ccei_reclass <- raster::reclassify(ccei, rcl_tbl_ccei, right = NA)

    raster::writeRaster(ccei_reclass,
                        file.path(out_folder,
                                  paste0("CCEI_reclass", scenario_name, ".tif")),
                        overwrite = overwrite, datatype = "INT2U")

    rm(ccei_reclass, ccei, brks_ccei)
  } else {
    rcl_tbl_ccei <- NULL
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
                          overwrite = overwrite, datatype = "INT2U")
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

      file_in <- raster::filename(dif_mt)
      if(file_in == ""){
        file_in <- raster::rasterTmpFile()
        raster::writeRaster(dif_mt, file_in)
      }

      # project raster to WGS84
      dif_mt <- wrap_gdalwarp(file_in, ref_crs,
                              raster::rasterTmpFile(),
                              resamp_method = "bilinear",
                              output_Raster = TRUE)
    }

    brs <- c(-1, 20.8, 26.3, 31.8, 50)

    rcl_tbl <- matrix(c(brs[1:4], brs[2:5], rev(1:4)), ncol = 3)

    dif_mt_reclass <- raster::reclassify(dif_mt, rcl_tbl, right = NA)

    raster::writeRaster(dif_mt_reclass,
                        file.path(out_folder, "MWMT_MCMT_reclass.tif"),
                        overwrite = overwrite, datatype = "INT2U")
  }

  # Climate data polygon boundary
  #does a clim_poly already exist in output folder
  clim_exists <- list.files(out_folder, pattern = "clim_poly.shp")
  if(length(clim_exists) > 0 && !overwrite){
    stop("A clim_poly already exists in out_folder. Set overwrite = TRUE or",
         " clim_poly = path/to/existing/climpoly", call. = FALSE)
  }

  if(is.null(clim_poly)){
    # make polygon boundary from raster data
    message("creating clim_poly from raster data")
    mat <- raster::raster(file.path(out_folder,
                                    paste0("MAT_reclass", scenario_name, ".tif")))
    mat <- raster::extend(mat, c(100,100), snap = "out")

    clim_bound <- raster::rasterToContour(is.na(mat), levels = 1)

    clim_poly <- clim_bound %>% sf::st_as_sf() %>%
      sf::st_polygonize() %>% sf::st_collection_extract("POLYGON") %>%
      sf::st_union() %>%
      sf::st_buffer(dist = 2 * raster::xres(mat))

  }
  sf::write_sf(clim_poly, file.path(out_folder, "clim_poly.shp"))

  message("finished processing")
  return(invisible(lst(brks_mat, brks_cmd, brks_ccei = rcl_tbl_ccei)))

}


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
#' @noRd
prep_exp <- function(rast_norm, rast_fut, file_nm, reproject = FALSE,
                     overwrite = FALSE, type = "halfIQR", brs = NULL){
  rast_delta <-  rast_norm - rast_fut

  if(reproject){
    ref_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    file_in <- raster::filename(rast_delta)
    if(file_in == ""){
      file_in <- raster::rasterTmpFile()
      raster::writeRaster(rast_delta, file_in)
    }
    # project raster to WGS84 and save to file_nm
    rast_delta <- wrap_gdalwarp(file_in, ref_crs,
                                overwrite = overwrite,
                                raster::rasterTmpFile(),
                                resamp_method = "bilinear",
                                output_Raster = TRUE)
  }

  if(type == "halfIQR"){
    sd_div <- 2
    type <- "IQR"
  } else if(type == "sd"){
    sd_div <- 1
  } else {
    stop("type should be halfIQR or sd not", type, call. = FALSE)
  }

  # returns the rcl table and writes raster to disk
  return(prep_from_delta(rast_delta, sd_div = sd_div, type = type,
                         file_nm = file_nm, overwrite = overwrite,
                         brs = brs))

}

#' Prepare exposure classes
#'
#' Classify the change in a climate variable into six categories.
#'
#' NatureServe uses the mean and the standard deviation to create classes in the
#' US but uses the mean and 1/2 the standard deviation and then shifts the
#' classes by one for temperature in Canada based on a visual interpretation of
#' the classes. To make a more reproducible process I use 1/2 the interquartile
#' range since it is more robust to outliers and skewed distributions.
#'
#' @param rast_delta raster of change in climate variable
#' @param sd_div number to divide standard deviation or interquartile range by
#' @param shift number of sd or IQRs to shift the breaks by can be 1 or -1
#' @param type "sd" for the mean and standard deviation (similar to
#'   NatureServe), "IQR" for the median and interquartile range (recommended),
#'   or "quantile" for six evenly spaced quantiles (not recommended)
#' @param brs breaks matrix to use. If not null type, shift and sd_div are ignored
#'
#' @noRd
prep_from_delta <- function(rast_delta, sd_div = 1, shift = 0, type = "sd",
                            file_nm, overwrite, brs = NULL){

  min_delta <- round(raster::cellStats(rast_delta, "min") -1, 3)

  max_delta <- round(raster::cellStats(rast_delta, "max") +1, 3)

  if(is.null(brs)){
    if(type == "sd"){
      mean_delta <- round(raster::cellStats(rast_delta, "mean"), 3)

      std_delta <- round(raster::cellStats(rast_delta, "sd")/sd_div, 3)

      brs <- c(min_delta, mean_delta-3*std_delta, mean_delta-2*std_delta,
               mean_delta-std_delta,
               mean_delta, mean_delta + std_delta, mean_delta + 2*std_delta,
               mean_delta + 3*std_delta, max_delta)
    } else if(type == "IQR"){
      med_delta <- round(stats::median(raster::sampleRegular(rast_delta, 1000000),
                                       na.rm = TRUE), 3)

      iqr_delta <- round(stats::IQR(raster::sampleRegular(rast_delta, 1000000),
                                    na.rm = TRUE)/sd_div, 3)

      brs <- c(min_delta, med_delta-3*iqr_delta, med_delta-2*iqr_delta,
               med_delta-iqr_delta,
               med_delta, med_delta + iqr_delta, med_delta + 2*iqr_delta,
               med_delta + 3*iqr_delta, max_delta)
    } else if(type == "quantile"){
      brs <- raster::quantile(rast_delta,
                              probs = seq(0, 1, 1/6))
      # make sure min and max included
      brs[1] <- brs[1] - 1
      brs[7] <- brs[7] + 1
    } else {
      stop("type must be one of sd, IQR or quantile not", type, call. = FALSE)
    }

    if(type == "quantile" && shift != 0){
      stop("shift must be 0 when type is quantile", call. = FALSE)
    }

    if(shift == 0){
      brs <- brs[c(1,3,4,5,6,7,9)]

      if(brs[6] > brs[7]){
        brs[7] <- brs[6]+1
      }

    } else if(shift == 1){
      brs <- brs[c(1,4,5,6,7,8,9)]
    } else if(shift == -1){
      brs <- brs[c(1,2,3,4,5,6,9)]
    } else {
      stop("shift must be 0, 1 or -1 not", shift, call. = FALSE)
    }

    rcl_tbl <- matrix(c(brs[1:6], brs[2:7], rev(1:6)), ncol = 3)
  } else {
    rcl_tbl <- brs

    rcl_tbl[1,1] <- min(min_delta, rcl_tbl[1,1])
    rcl_tbl[nrow(rcl_tbl),2] <- max(max_delta, rcl_tbl[nrow(rcl_tbl),2])
  }

  raster::reclassify(rast_delta, rcl_tbl, right = NA, filename = file_nm,
                     overwrite = overwrite, datatype = "INT2U")

  return(rcl_tbl)
}

check_crs <- function(rast){
  if(is.na(raster::crs(rast))){
    stop("the raster ", raster::filename(rast),
         " has no CRS. Please provide a file with CRS information",
         call. = FALSE)
  }
}
