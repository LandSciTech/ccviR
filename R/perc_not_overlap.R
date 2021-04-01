# get the proportion of a polygon that does not overlap non-NA values of raster
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
perc_not_overlap <- function(rast, poly, var_name){
  # # First check if poly is 100% inside rast extent and if so assume full overlap
  # rast_bbox <- st_bbox(rast) %>% st_as_sfc()
  # contained <- st_contains(rast_bbox, poly, sparse = FALSE)
  #
  # if(contained[1,1]){
  #   return(tibble(x = 100) %>% set_names(var_name))
  # }
  # rast_poly <- spex::polygonize(rast)
  # rast_poly <- st_union(rast_poly)
  # dif <- st_difference(non_breed_poly, rast_poly)
  #
  # dif_area <- st_area(dif)
  # poly_area <- st_area(poly)
  # prop_area <- dif_area/poly_area * 100
  # out <- tibble(x = prop_area) %>% set_names(var_name)
  # return(out)
  # # user  system elapsed
  # # 151.00    2.00  153.25

  # Another option using mask will be less accurate near the poles but is much
  # faster
  r_crop <- raster::crop(rast, poly)
  r_mask <- raster::mask(r_crop, poly, updatevalue = NA, updateNA= TRUE)
  cells_overlap <- raster::freq(r_mask, useNA = "no")[,2] %>% sum()
  area_cell <- raster::area(r_mask, na.rm = TRUE) %>%
    raster::cellStats("mean")

  area_overlap <- cells_overlap * area_cell *1000000

  poly_area <- st_area(poly) %>% units::set_units(NULL)

  prop_area <- (poly_area - area_overlap)/poly_area * 100

  out <- tibble(x = prop_area) %>% purrr::set_names(var_name)
  return(out)

}