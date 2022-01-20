#' Load climate variables
#'
#' Load climate variables and store them in a list that can be used with
#' \code{\link{run_spatial}}. The climate variables should first be prepared
#' using \code{\link{run_prep_data}}.
#'
#' @param root_pth A folder location where all the climate data is stored. The
#'   names must match one of \code{c("MAT.*tif$", "CMD.*tif$", "clim_poly.*shp",
#'   "MAP.*tif$", "ccei.*tif$|CCEI.*tif$","MWMT.*tif$|HTN.*tif$")} and the first
#'   three are required.
#'
#' @return A list of climate variables with names "mat", "cmd", "map", "ccei",
#'   "htn", "clim_poly"
#'
#' @export
#'
#' @examples
#' pth <- system.file("extData/clim_files/processed", package = "ccviR")
#'
#' get_clim_vars(pth)

get_clim_vars <- function(root_pth){
  if(!dir.exists(root_pth)){
    stop("directory ", root_pth," does not exist", call. = FALSE)
  }

  pats <- c("MAT.*tif$", "CMD.*tif$", "MAP.*tif$", "ccei.*tif$|CCEI.*tif$",
            "MWMT.*tif$|HTN.*tif$", "clim_poly.*shp")
  err <- c(T, T, F, F, F, T)

  clim_vars <- purrr::map2(pats, err, ~check_clim(root_pth, .x, .y)) %>%
    purrr::map(load_clim) %>%
    purrr::set_names(c("mat", "cmd", "map", "ccei", "htn", "clim_poly"))

  return(clim_vars)
}

check_clim <- function(root_pth, pattern, error){
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
    stop("More than one file matching the expression: ", pattern,
         ". Please remove duplicates",
         call. = FALSE)
  }
  pth
}

load_clim <- function(pth){
  if(is.null(pth)){
    return(NULL)
  }
  out <- tryCatch({
    if(raster::extension(pth) == ".shp"){
      out <- st_read(pth, agr = "constant", quiet = TRUE)

      if(nrow(out) > 1){
        out <- st_union(out) %>% st_as_sf()
      }
      return(out)
    } else {
      out <- raster::raster(pth)
    }
  },
  error = function(cond){
    message(pth)
    stop(cond)
  })

  out <- check_trim(out)

  out

}
#' Trim NAs from raster
#'
#' Copied from raster package internal .memtrimlayer in raster::trim. This is
#' not memory safe but raster::trim takes over an hour while this takes ~30s
#'
#' @param r raster to be trimmed
#' @param padding integer. Number of outer rows/columns to keep
#' @param values numeric. Value(s) based on which a Raster* should be trimmed
#' @param filename character. Optional output filename
#' @param ... If x is a Raster* object: additional arguments as for writeRaster
#'
#' @return the trimmed Raster* object
#'
#' @examples
#' library(raster)
#'
#' rast <- raster(matrix(NA, nrow = 100, ncol = 100))
#'
#' rast[30:60, 30:60] <- 1
#'
#' trim_ras(rast)
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

check_trim <- function(rast){
  do_trim <- sum(!is.na(rast[1:10,]))
  if(do_trim == 0){
    message("doing trim")
    rast <- trim_ras(rast)
  }
  return(rast)
}

