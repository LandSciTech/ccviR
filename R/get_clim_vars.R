#' Load climate variables
#'
#' Load climate variables and store them in a list that can be used with
#' \code{\link{analyze_spatial}}. The climate variables should first be prepared
#' using \code{\link{prep_clim_data}}.
#'
#' @param root_pth A folder location where all the climate data is stored. The
#'   names must match one of \code{c("MAT.*tif$", "CMD.*tif$", "clim_poly.*shp",
#'   "MAP.*tif$", "ccei.*tif$|CCEI.*tif$","MWMT.*tif$|HTN.*tif$")} and the first
#'   three are required.
#' @param scenario_names character vector with names that identify multiple
#'   future climate scenarios. If this is supplied the raster file must include
#'   the scenario name as a suffix to the pattern mentioned above eg. if there
#'   are two MAT files "MAT_RCP 4.5.tif" and "MAT_RCP 8.5.tif" the scenario names
#'   should be "RCP 4.5" and "RCP 8.5". This will happen automatically if the
#'   scenario name is provided to \code{\link{prep_clim_data}}.
#'
#' @return A list of climate variables with names "mat", "cmd", "map", "ccei",
#'   "htn", "clim_poly". If multiple scenarios are used mat, cmd and ccei will
#'   be SpatRasters with one layer per scenario.
#'
#' @export
#'
#' @examples
#' pth <- system.file("extdata/clim_files/processed", package = "ccviR")
#'
#' # scenario names
#' scn_nms <- c("RCP 4.5", "RCP 8.5")
#'
#' get_clim_vars(pth, scn_nms)

get_clim_vars <- function(root_pth, scenario_names = "scn1"){
  if(!dir.exists(root_pth)){
    stop("directory ", root_pth," does not exist", call. = FALSE)
  }

  # remove spaces from scenario_names
  scenario_names <- stringr::str_replace_all(scenario_names, "\\s", "_")

  pats <- c("MAT.*tif$", "CMD.*tif$", "MAP.*tif$", "ccei.*tif$|CCEI.*tif$",
            "MWMT.*tif$|HTN.*tif$", "clim_poly.*shp")
  err <- c(T, T, F, F, F, T)

  clim_vars <- purrr::map2(pats, err, ~check_clim(root_pth, .x, .y, scenario_names)) %>%
    purrr::map(load_clim, scenario_names) %>%
    purrr::set_names(c("mat", "cmd", "map", "ccei", "htn", "clim_poly"))

  return(clim_vars)
}

check_clim <- function(root_pth, pattern, error, scenario_names = "scn1"){
  pth <- list.files(root_pth, pattern = pattern, full.names = TRUE)

  if(length(pth) == 0){
    if(error){
      stop("There is no file in ", root_pth, " matching the expression ", pattern,
           ". Please add this file or rename it.",
           call. = FALSE)
    } else {
      return(NULL)
    }
  }

  if(length(pth) > 1){
    if(length(pth) != length(scenario_names)){
      stop("The number of files matching the expression: ", pattern,
           " does not match the number of scenario names. ",
           "Please provide one file per scenario",
           call. = FALSE)
    }
  }

  pth
}

load_clim <- function(pth, scenario_names = "scn1"){

  if(length(pth) > 1){
    # make sure the order of pth matches scenario_names
    pth2 <- purrr::map(scenario_names, ~grep(.x, pth, value = TRUE)) %>% unlist()

    if(length(pth2) != length(pth)){
      stop("the filename ", setdiff(pth, pth2),
           " does not match any of the scenario_names. ",
           paste0(scenario_names, collapse = ", "),
           call. = FALSE)
    }

    out <- purrr::map(pth2, load_clim) %>% purrr::set_names(scenario_names)

    out <- terra::rast(out)

    out <- check_trim(out)

    return(out)
  }

  if(is.null(pth)){
    return(NULL)
  }
  out <- tryCatch({
    if(fs::path_ext(pth) == "shp"){
      out <- st_read(pth, agr = "constant", quiet = TRUE)

      if(nrow(out) > 1){
        out <- st_union(out) %>% st_as_sf()
      }

      if(is.na(st_crs(out))){
        stop("The file at ", pth, " does not have a CRS.",
             " \nPlease load a file with a valid Coordinate Reference System",
             call. = FALSE)
      }
      return(out)
    } else {
      out <- terra::rast(pth)
      if(is.na(terra::crs(out))||terra::crs(out) == ""){
        stop("The file at ", pth, " does not have a CRS.",
             " \nPlease load a file with a valid Coordinate Reference System",
             call. = FALSE)
      }
      return(out)
    }
  },
  error = function(cond){
    message("Erorr loading:", pth)
    stop(cond)
  })

  out <- check_trim(out)



  out

}
#' Trim NAs from raster
#'
#' Trim NAs from raster if the first 10 rows are all NA
#'
#' @param r raster to be trimmed
#' @param padding integer. Number of outer rows/columns to keep
#' @param values numeric. Value(s) based on which raster should be trimmed
#' @param filename character. Optional output filename
#' @param ... additional arguments as for writeRaster
#'
#' @return the trimmed SpatRaster object
#'
#' @examples
#' library(terra)
#'
#' ras <- rast(matrix(NA, nrow = 100, ncol = 100))
#'
#' ras[30:60, 30:60] <- 1
#'
#' trim_ras(ras)
#' @noRd
#'

# previously had used this from a raster internal for small rasters which was
# fast but not memory safe
# trim_ras <- utils::getFromNamespace(".memtrimlayer", ns = "raster")

check_trim <- function(ras){
  do_trim <- sum(!is.na(ras[1:10,]))
  if(do_trim == 0){
    message("doing trim")
    ras <- terra::trim(ras)
  }
  return(ras)
}

