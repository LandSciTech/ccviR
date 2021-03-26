#' Get the proportion of each class in a raster that overlaps a polygon
#'
#' @param rast
#' @param poly
#' @param var_name
#' @param scope
#' @param val_range
#' @param eer_pkg
#'
#' @export
calc_prop_raster <- function(rast, poly, var_name, scope, val_range = 1:6,
                             eer_pkg = requireNamespace("exactextractr",
                                                        quietly = TRUE)){
  if(eer_pkg){
    out <- exactextractr::exact_extract(rast, poly, progress = FALSE)
    out <- out[[1]] %>%
      filter(!is.na(value)) %>%
      mutate(value = factor(value, levels = val_range)) %>%
      group_by(value, .drop = FALSE) %>%
      summarise(sum = sum(coverage_fraction)) %>%
      transmute(value, prop = (sum/sum(sum) * 100) %>% round(3))

  } else {
    message("install package exactextractr for much faster execution")
    out <- raster::extract(rast, poly,  df = TRUE)
    out <- prop.table(table(out[,2]))
    out <- round(100 * out, 3)
    out <- as.data.frame(out) %>% purrr::set_names("value", "prop")
  }

  out <- tidyr::pivot_wider(out, names_from = "value", values_from = "prop",
                     names_prefix = paste0(var_name, "_"))
}
