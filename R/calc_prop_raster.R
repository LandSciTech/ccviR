#' Get the proportion of each class in a raster that overlaps a polygon
#'
#' @param rast
#' @param poly
#' @param var_name
#' @param val_range
#'
#' @noRd
calc_prop_raster <- function(rast, poly, var_name, val_range = 1:6, digits = 3,
                             check_overlap = 0.99, return_overlap_as = NULL){
  withCallingHandlers(
    warning = function(cnd){
      if(grepl("transformed to raster", conditionMessage(cnd))){
        message("Polygons were transformed to have CRS matching raster")
        invokeRestart("muffleWarning")
      }
    },
    ext_out <- exactextractr::exact_extract(rast, poly, progress = FALSE, include_area = TRUE)
  )
  poly_area <- st_area(poly)
  out <- ext_out[[1]] %>% select(-area) %>%
    setNames(nm = c(names(rast), "coverage_fraction")) %>%
    filter(if_any(.fun = ~!is.na(.x))) %>%
    tidyr::pivot_longer(cols = c(-coverage_fraction), names_to = "layer",
                        values_to = "value") %>%
    mutate(value = factor(value, levels = val_range)) %>%
    group_by(layer, value, .drop = FALSE) %>%
    summarise(sum = sum(coverage_fraction)) %>%
    transmute(value, prop = (sum/sum(sum) * 100) %>% round(digits)) %>%
    ungroup()

  out <- tidyr::pivot_wider(out, names_from = "value", values_from = "prop",
                            names_prefix = paste0(var_name, "_")) %>%
    arrange(factor(layer, levels = names(rast))) %>%
    select(-layer)

  if(check_overlap > 0 || !is.null(return_overlap_as)){
    cov_area <- ext_out[[1]] %>% mutate(area_frac = area*coverage_fraction) %>%
      pull(area_frac) %>% sum()

    prop_cov <- cov_area/units::drop_units(poly_area)

    if(!is.null(return_overlap_as)){
      prop_out <- data.frame(x = prop_cov) %>% setNames(return_overlap_as)

      out <- bind_cols(out, prop_out)
    }

    if(prop_cov < check_overlap){
      stop("The range polygon does not fully overlap the supplied ", var_name,
           " raster.", call. = FALSE)
    }
  }

  out
}
