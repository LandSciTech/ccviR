#' Get the proportion of each class in a raster that overlaps a polygon
#'
#' @param rast raster
#' @param poly polygon
#' @param var_name variable name for output
#' @param val_range vector of possible values in raster
#' @param digits number of digits to round proportion to
#' @param check_overlap proportion of overlap below which there should be an error
#' @param return_overlap_as column name for percent overlap in the returned dataframe
#'
#' @return a data.frame
#'
#' @noRd
calc_prop_raster <- function(rast, poly, var, var_name = NULL, val_range = 1:6, digits = 3,
                             check_overlap = 0.99, return_overlap_as = NULL,
                             quiet = FALSE) {

  if(is.null(var_name)) var_name <- var

  # Debugging invalid protected areas (use `TRUE` to debug and explore)
  if(FALSE && var_name == "protected") {
    browser()
    p <- sf::st_cast(poly, "MULTIPOLYGON") %>%
      sf::st_cast("POLYGON")
    i <- sf::st_is_valid(p)
    nv <- which(!i)
    length(nv)
    nrow(p)
    plot(p[nv,], col = "yellow")
    sf::st_is_valid(p[nv,], reason = TRUE)
  }

  if(st_crs(poly) != st_crs(rast)) {
    poly <- st_transform(poly, st_crs(rast))
    inform_prog(
      paste0("Calculating raster overlap: Polygons transformed to match CRS of '",
             var, "' raster"),
      quiet)
  }

  ext_out <- exactextractr::exact_extract(rast, poly, progress = FALSE, include_area = TRUE)

  out <- ext_out[[1]] %>%
    select(-"area") %>%
    stats::setNames(nm = c(names(rast), "coverage_fraction")) %>%
    filter(if_any(.cols = everything(), .fns = ~!is.na(.x))) %>%
    tidyr::pivot_longer(cols = c(-"coverage_fraction"), names_to = "layer",
                        values_to = "value") %>%
    mutate(value = factor(.data$value, levels = val_range)) %>%
    group_by(.data$layer, .data$value, .drop = FALSE) %>%
    summarise(sum = sum(.data$coverage_fraction), .groups = "drop_last") %>%
    transmute(.data$value, prop = (.data$sum/sum(.data$sum) * 100) %>% round(digits)) %>%
    ungroup()

  out <- tidyr::pivot_wider(out, names_from = "value", values_from = "prop",
                            names_prefix = paste0(var_name, "_")) %>%
    arrange(factor(.data$layer, levels = names(rast))) %>%
    select(-"layer")

  if(check_overlap > 0 || !is.null(return_overlap_as)){
    cov_area <- ext_out[[1]] %>%
      mutate(area_frac = .data$area*.data$coverage_fraction) %>%
      pull(.data$area_frac) %>%
      sum()

    poly_area <- sum(st_area(poly))

    prop_cov <- cov_area/units::drop_units(poly_area)

    if(!is.null(return_overlap_as)) {
      prop_out <- data.frame(x = prop_cov) %>%
        stats::setNames(return_overlap_as)
      out <- bind_cols(out, prop_out)
    }

    if(prop_cov < check_overlap){
      stop("The polygon does not fully overlap the supplied ", var,
           " raster.", call. = FALSE)
    }
  }

  out
}
