# Get the min and max of raster that overlaps a polygon
#' Title
#'
#' @param rast
#' @param poly
#' @param var_name
#' @param eer_pkg
#'
#' @return
#' @export
#'
#' @examples
calc_min_max_raster <- function(rast, poly, var_name,
                                eer_pkg = requireNamespace("exactextractr",
                                                           quietly = TRUE)){
  if(eer_pkg){
    out <- exactextractr::exact_extract(rast, poly, progress = FALSE)
    out_min <- out[[1]] %>% pull(value) %>% min(na.rm = TRUE)
    out_max <- out[[1]] %>% pull(value) %>% max(na.rm = TRUE)
  } else {
    out <- raster::extract(rast, poly,  df = TRUE)
    out_min <- min(out[,2], na.rm = TRUE)
    out_max <- max(out[,2], na.rm = TRUE)
  }

  out <- tibble(max = out_max, min = out_min) %>%
    purrr::set_names(paste0(var_name, "_max"), paste0(var_name, "_min"))

}
