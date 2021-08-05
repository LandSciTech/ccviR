#' Load climate variables
#'
#' Load climate variables that will be the same across most species.
#'
#' @export

get_clim_vars <- function(root_pth){
  pats <- c("MAT.*tif$", "CMD.*tif$", "MAP.*tif$", "ccei.*tif$|CCEI.*tif$",
            "MWMT.*tif$|HTN.*tif$", "PTN.*shp$")
  err <- c(T, T, F, F, F, F)

  clim_vars <- purrr::map2(pats, err, ~check_clim(root_pth, .x, .y)) %>%
    purrr::map(load_clim) %>%
    purrr::set_names(c("mat", "cmd", "map", "ccei", "htn", "ptn"))
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
#' Trim NAs from raster copied from raster package internal .memtrimlayer in
#' raster::trim. This is not memory safe but raster::trim takes over an hour
#' while this takes ~30s
#' @param x
#'
#' @param padding
#' @param values
#' @param filename
#' @param ...
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

#' @export
check_trim <- function(rast){
  do_trim <- sum(!is.na(rast[1:10,]))
  if(do_trim == 0){
    message("doing trim")
    rast <- trim_ras(rast)
  }
  return(rast)
}

