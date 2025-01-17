
#' Prepare Climate Change Exposure Index raster
#'
#' Using the Standardized Euclidean Distance method from Williams et al., 2007
#'
#' @inheritParams common_docs
#'
#' @references
#'   J.W. Williams, S.T. Jackson, J.E. Kutzbach, Projected distributions of
#'   novel and disappearing climates by 2100 AD, Proc. Natl. Acad. Sci. U.S.A.
#'   104 (14) 5738-5742, https://doi.org/10.1073/pnas.0606292104 (2007).
#'
#' @returns
#' @export
#'
#' @examples
#' prep_ccei()
#'
prep_ccei <- function(path_hist = "misc/ccei/historical",
                      path_future = "misc/ccei/future") {
#  - SED equation from Williams et al., 2007
#  - CMD equation from Wang et al. 2012
#  - But requires a value of Extra Terrestrical Radiation (Ra) in mm/day
#  - ASK climr GROUP TO EXPORT, OTHERWISE WE'LL INCLUDE WITH LICENSE

  # Calculate annual CMD and Tmean for historical data
  ccei_annual()

}


#' Calculate Annual Climate Moisture Deficit and Mean Temperature
#'
#' Calculate Annual CMD and Mean Temperature for historical values in
#' preparation for creating the Climate Exposure Index Raster.
#'
#' Annual CMD = Sum of Monthly CMD (difference between Eref and monthly
#' precipitation)
#'
#' Annual Mean Temperature = Mean of monthly average temperature
#' (Midpoint: (Monthly maximum temp - Monthly minimum temp) / 2)
#'
#' @inheritParams common_docs
#' @returns Annual CMD raster tif and Annual Mean Temperature raster tif saved
#'   to an 'intermediate' folder in the `path_hist`.
#' @export
#'
#' @examples
#' ccei_annual()

ccei_annual <- function(path_hist = "misc/ccei/historical", quiet = FALSE) {

  # TODO: Temp for testing - REMOVE
  path_hist <- "misc/ccei/historical"

  # Output files
  out_dir <- fs::path(fs::path_dir(path_hist), "intermediate")
  fs::dir_create(out_dir)
  out_cmd <- fs::path(out_dir, "all_years_cmd.tiff")
  out_tmean <- fs::path(out_dir, "all_years_tmean.tiff")

  # Get boundaries of western hemisphere
  clip <- ccei_clip()

  # Get raster files and combine layers by month
  hist <- dplyr::tibble(file = fs::dir_ls(path_hist, regexp = "\\.tiff?$")) %>%
    dplyr::mutate(
      year = stringr::str_extract(file, "\\d{4}"),
      month = as.numeric(stringr::str_extract(file, "(?<=\\d{4}-)\\d{2}"))) %>%
    dplyr::summarize(
      raster = list(terra::rast(file)),
      raster = purrr::map(
        raster,
        ~stats::setNames(.x, stringr::str_extract(names(.x), "prec|tmin|tmax"))),
      .by = c("year", "month"))

  # Took ~ 30-35min to run all 40 years
  all <- purrr::map(unique(hist$year), ~ {
    rlang::inform(paste0(.x, " - ", Sys.time()))
    h <- filter(hist, year == .x)
    h1 <- purrr::map2(h$raster, h$month,
                      ~ccei_monthly(.x, .y, clip, quiet = quiet))
    cmd <- purrr::map(h1, ~.x[["cmd"]]) %>%
      terra::rast() %>%
      sum() %>%
      setNames("cmd")
    tmean <- purrr::map(h1, ~.x[["tmean"]]) %>%
      terra::rast() %>%
      terra::mean(na.rm = TRUE) %>%
      setNames("tmean")

    c(cmd, tmean)
  }, .progress = !quiet)

  all_cmd <- terra::rast(purrr::map(all, ~.x[["cmd"]]))
  all_tmean <- terra::rast(purrr::map(all, ~.x[["tmean"]]))
  terra::writeRaster(all_cmd, out_cmd)
  terra::writeRaster(all_tmean, out_tmean)
}





#' Calculate monthly Climate Moisture Deficit and Mean Temperature
#'
#' Because climr:::calc_Eref() and and climr:::calc_cmd are expected to work
#' on vectors, they don't work with a raster in memory.
#'
#' terra::app() is very slow as it applies a function to every cell, where as
#' even if we extract the raster values to memory, being able to apply things in
#' parallel is much faster.
#'
#' Therefore we extract raster values calculate the Eref and CMD and then return
#' as a raster.
#'
#' @param r SpatRaster. Historical values for one month, `prec`, `tmin`, `tmax`.
#' @param month Numeric. Which month?
#' @param clip SpatExtent. Area which to clip the raster to.
#' @param quiet Logical. Silence progress messages.
#'
#' @returns SpatRaster with `tmean` and `cmd`

ccei_monthly <- function(r, month, clip, quiet) {
  if(!quiet) rlang::inform(paste0("  ", month))

  # Prep for calculations
  r <- terra::crop(r, clip)
  r <- c(r, setNames(terra::init(r, "y"), "latitude")) # Add latitude for Eref

  # Extract values
  df <- terra::values(r)

  # Calculate eref, cmd, tmean
  # TODO: Use exported climr functions when available
  eref <- climr:::calc_Eref(
    month,
    tmmin = df[,"tmin"],
    tmmax = df[,"tmax"],
    latitude = df[,"latitude"])
  r$cmd <- climr:::calc_CMD(eref, df[,"prec"])
  r$eref <- eref
  r$tmean <- (df[, "tmax"] - df[,"tmin"])/2

  # Keep only new layers
  r[[-which(names(r) %in% c("prec", "tmax", "tmin"))]]
}




ccei_clip <- function() {
  rnaturalearth::ne_countries(continent = c("North America", "South America")) %>%
    sf::st_buffer(units::set_units(100, "km")) %>%
    terra::vect() %>%
    terra::ext()
}

#' Standard deviation of the interannual variability
#'
#' @returns
#' @noRd
calc_sd <- function() {

  # NOTE: Using default, pop = TRUE, for Population SD
  terra::stdev(r)

}


#' Standardized Euclidean Distance for local climate
#'
#' From Williams et al. 2007, calculate the Standardized Euclidean Distance
#' (SED) for local climate change for a series of raster tiles.
#'
#' Each named vector represents a different climate variable Names must be
#' consistent among `b`, `a`, and `s`. Each value represents the value for that
#' climate variable for a given raster tile.
#'
#' @param b Named List of Numeric Vectors. Vectors of values for Future climate variables.
#' @param a Named List of Numeric Vectors. Vectors of values for Historical climate variables.
#' @param s Named List of Numeric vectors. Each list item represents a vector for a
#'   different climate variable. Values are standard deviations of the
#'   interannual variability for historical climate variable.
#'
#' @returns
#' @export
#'
#' @examples
calc_sed <- function(b, a, s) {

  purrr::pmap(list(b, a, s), (b - a)^2 / s^2) %>%
    purrr::pmap(~sqrt(sum(..1, ..2)))

}

