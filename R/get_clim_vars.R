#' Load climate variables
#'
#' Load climate variables that will be the same across most species.

get_clim_vars <- function(root_pth){
  pats <- c("MAT.*tif$", "CMD.*tif$", "MAP.*tif$", "ccei.*tif$",
            "MWMT.*tif$", "PTN.*shp$")
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
  pth
}

load_clim <- function(pth){
  if(is.null(pth)){
    return(NULL)
  }
  if(raster::extension(pth) == ".shp"){
    out <- st_read(pth, agr = "constant", quiet = TRUE)

    if(nrow(out) > 1){
      out <- st_union(out) %>% st_as_sf()
    }
  } else {
    out <- raster::raster(pth)
  }
  out

}
