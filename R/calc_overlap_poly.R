#' get the proportion of poly1 that overlaps poly2
#'
#' @noRd
calc_overlap_poly <- function(poly1, poly2, var_name){
  int1_2 <- st_intersection(poly1, poly2)

  int1_2 <- st_set_agr(int1_2, "constant")

  int1_2 <- valid_or_error(int1_2, paste0(var_name, " intersection"))

  if(nrow(int1_2) == 0){
    out <- tibble(x = 0) %>% purrr::set_names(var_name)
    return(out)
  } else {
    int1_2 <- st_area(int1_2) %>% units::set_units(NULL)
    area1 <- st_area(poly1) %>% units::set_units(NULL)
    prop_area <- int1_2/area1 * 100
    out <- tibble(x = prop_area) %>% purrr::set_names(var_name)
    return(out)
  }
}
