
#' Calculate the proportion of a raster that overlaps another raster
#'
#' @noRd
#'
#' @examples
#' # Get and prepare example data
#' d <- test_data()
#'
#' r1 <- terra::crop(d$protected_rast,
#'                   st_transform(d$assess_poly, terra::crs(d$protected_rast)))
#'
#' r2 <- terra::subst(d$rng_chg_rast, -1, NA)
#'
#' # Calculate overlap
#' calc_overlap_raster(r1, r2)

calc_overlap_raster <- function(rast1, rast2) {

  # Align by raster with smallest resolution
  if(terra::res(rast1)[1] < terra::res(rast2)[1]) {
    r1 <- rast1
    r2 <- terra::project(rast2, rast1)
  } else {
    r1 <- terra::project(rast1, rast2)
    r2 <- rast2
  }

  # Find areas of intersection
  r <- terra::intersect(r1, r2)

  # DEBUG - Compare overlaps
  #coltab(r2) <- data.frame(value = c(0, 1), col = c("#FF000050", "#FF000050"))
  #terra::plot(r[[1]], reset = FALSE, col = "purple")
  #terra::plot(r1[[1]], add = TRUE, alpha = 0.5)

  # Calculate proportion of overlap
  r <- terra::global(r, c("sum", "notNA"), na.rm = TRUE) %>%
    as.data.frame() %>%
    mutate(protected = sum / notNA * 100) %>%
    select("protected")
  rownames(r) <- NULL

  return(r)
}
