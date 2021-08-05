# Get the min and max of raster that overlaps a polygon
#' Title
#'
#' @param rast
#' @param poly
#' @param var_name
#'
#' @return
#' @export
#'
#' @examples
calc_min_max_raster <- function(rast, poly, var_name){

  withCallingHandlers(
    warning = function(cnd){
      if(grepl("transformed to raster", conditionMessage(cnd))){
        message("Polygons were transformed to have CRS matching raster")
        invokeRestart("muffleWarning")
      }
    },
    out <- exactextractr::exact_extract(rast, poly, progress = FALSE)
  )
  out_min <- out[[1]] %>% pull(value) %>% min(na.rm = TRUE)
  out_max <- out[[1]] %>% pull(value) %>% max(na.rm = TRUE)

  out <- tibble(max = out_max, min = out_min) %>%
    purrr::set_names(paste0(var_name, "_max"), paste0(var_name, "_min"))

}
