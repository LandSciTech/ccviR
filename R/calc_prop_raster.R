#' Get the proportion of each class in a raster that overlaps a polygon
#'
#' @param rast
#' @param poly
#' @param var_name
#' @param val_range
#'
#' @export
calc_prop_raster <- function(rast, poly, var_name, val_range = 1:6, digits = 3){
  withCallingHandlers(
    warning = function(cnd){
      if(grepl("transformed to raster", conditionMessage(cnd))){
        message("Polygons were transformed to have CRS matching raster")
        invokeRestart("muffleWarning")
      }
    },
    out <- exactextractr::exact_extract(rast, poly, progress = FALSE)
  )
  out <- out[[1]] %>%
    filter(!is.na(value)) %>%
    mutate(value = factor(value, levels = val_range)) %>%
    group_by(value, .drop = FALSE) %>%
    summarise(sum = sum(coverage_fraction)) %>%
    transmute(value, prop = (sum/sum(sum) * 100) %>% round(digits))

  out <- tidyr::pivot_wider(out, names_from = "value", values_from = "prop",
                            names_prefix = paste0(var_name, "_"))
}
