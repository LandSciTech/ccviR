
#' Prepare Climate Change Exposure Index raster
#'
#' Using the Standardized Euclidean Distance method from Williams et al., 2007
#' and NatureServe v3.02.1 2016.
#'
#' @inheritParams common_docs
#'
#' @references
#'   J.W. Williams, S.T. Jackson, J.E. Kutzbach, Projected distributions of
#'   novel and disappearing climates by 2100 AD, Proc. Natl. Acad. Sci. U.S.A.
#'   104 (14) 5738-5742, https://doi.org/10.1073/pnas.0606292104 (2007).
#'
#'   B.E. Young, E. Byers, G. Hammerson, A. Frances, L. Oliver, A. Treher,
#'   Guidelines for Using the NatureServe Climate Change Vulnerability Index.
#'   Release 3.02. https://www.natureserve.org/sites/default/files/guidelines_natureserveclimatechangevulnerabilityindex_r3.02_1_jun_2016.pdf (June 1st 2016)
#'
#' @returns Creates rasters and saves them to `path_ccei`. The final CCEI rasters are
#' saved as `ccei_ssp245.tif` and `ccei_ssp585.tif`.
#' @export
#'
#' @examples
#' \dontrun{
#'   prep_ccei()
#' }

prep_ccei <- function(path_ccei = "misc/ccei",
                      overwrite = TRUE, quiet = FALSE) {
#  - SED equation from Williams et al., 2007
#  - CMD equation from Wang et al. 2012
#  - But requires a value of Extra Terrestrical Radiation (Ra) in mm/day
#  - ASK climr GROUP TO EXPORT, OTHERWISE WE'LL INCLUDE WITH LICENSE
  prep_ccei_historical(path_ccei, overwrite, quiet)
  prep_ccei_future(path_ccei, overwrite, quiet)
  if(!quiet) rlang::inform("Calculating CCEI")
  calc_ccei(path_ccei, scenario = "ssp245", overwrite = overwrite, quiet = quiet)
  calc_ccei(path_ccei, scenario = "ssp585", overwrite = overwrite, quiet = quiet)
}

#' Prepare Historical Data for Climate Change Exposure Index
#'
#' Calculate CMD and Tmean from monthly data, calculate annual values (see
#' `ccei_annual()`). Combine and calculate mean and interannual standard
#' deviations for each raster cell over the entire historical record.
#'
#' @inheritParams common_docs
#'
#' @returns Writes intermediate and final rasters to `path_ccei` in the
#'   "Intermediate" folder. Annual means (`hist_YYYY.tif` and
#'   `hist_groups_VAR.tif`)as well as overall historical averages and standard
#'   deviations `hist_all_vars.tif`.
#' @export
#'
#' @examples
#' \dontrun{
#'   prep_ccei_historical()
#' }

prep_ccei_historical <- function(path_ccei = "misc/ccei",
                                 overwrite = TRUE, quiet = FALSE) {

  out <- prep_out(path_ccei, "intermediate", "hist")
  rasts <- combine_historical(path_ccei)

  # Calculate annual CMD and Tmean for historical data
  # - Calculate overall mean CMD and mean TMean, and then sd as well
  # - Took ~ 10 min to run all 40 years (down from 30min!)

  ccei_values(rasts, out, aggregate = TRUE, overwrite = overwrite, quiet = quiet)
}

combine_historical <- function(path_ccei = "misc/ccei") {

  rasts <- fs::path(path_ccei, "historical") %>%
    fs::dir_ls(regexp = "\\.tiff?$") %>%
    dplyr::tibble(file = .) %>%
    dplyr::mutate(
      year = stringr::str_extract(.data$file, "\\d{4}"),
      month = as.numeric(stringr::str_extract(.data$file, "(?<=\\d{4}-)\\d{2}")),
      var = stringr::str_extract(.data$file, "prec|tmin|tmax"),
      group = .data$year)

  return(rasts)
}


#' Prepare Future Data for Climate Change Exposure Index
#'
#' Calculate CMD and Tmean from monthly data averages.
#'
#' @inheritParams common_docs
#'
#' @returns Writes rasters to `path_ccei` in the "Intermediate" folder. Overall
#'   averages for each model/scenario `future_MODEL-SCENARIO.tif`.
#' @export
#'
#' @examples
#' \dontrun{
#'   prep_ccei_future()
#' }

prep_ccei_future <- function(path_ccei = "misc/ccei", overwrite = TRUE, quiet = FALSE) {

  out <- prep_out(path_ccei, "intermediate", "future")
  rasts <- combine_future(path_ccei, quiet)

  # Calculate annual CMD and Tmean for future data
  # - Calculate overall mean CMD and mean TMean, but do not require SD
  # - Took ~ 10 min to run all 40 years (down from 30min!)
  ccei_values(rasts, out, overwrite = overwrite, quiet = quiet)
}

combine_future <- function(path_ccei = "misc/ccei", quiet = FALSE) {

  # Get raster files and combine layers by model and scenario
  rasts <- fs::path(path_ccei, "future") %>%
    fs::dir_ls(regexp = "\\.tiff?$") %>%
    dplyr::tibble(file = .) %>%
    dplyr::mutate(
      model = stringr::str_extract(.data$file, "(?<=_)[A-Za-z0-9-]+(?=_ssp)"),
      ssp = stringr::str_extract(.data$file, "ssp(245|585)"),
      var = stringr::str_extract(.data$file, "prec|tmin|tmax"),
      group = paste0(.data$model, "-", .data$ssp)) %>%
    arrange(.data$model, .data$ssp)

  # Report models and scenarios used
  if(!quiet) {
    rlang::inform(c(
      paste0("Using models (n = ", dplyr::n_distinct(rasts$model), ") for ",
             paste0(unique(rasts$ssp), collapse = " and "), ":"),
      unique(rasts$model)))
  }

  # Check potential missing data
  should_have <- dplyr::n_distinct(rasts$model) * dplyr::n_distinct(rasts$ssp) * 3
  have <- nrow(rasts)
  if(have != should_have) {
    rlang::abort(
      c("Missing data for some variable, model, and ssp combinations",
        paste("Only have", have, "files, should have", should_have, "files"),
        "We need 3 variables ('prec', 'tmin', 'tmax') for each model/ssp combo"
      ),
      call = NULL)
  }

  return(rasts)
}


#' Calculate Annual Climate Moisture Deficit and Mean Temperature
#'
#' Calculate Annual CMD and Mean Temperature in
#' preparation for creating the Climate Exposure Index Raster.
#'
#' @param rasts Data.frame of rasters from `prep_ccei_historical()` or
#'   `prep_ccei_future()`
#' @param out Character. Out folder/file preface
#' @param aggregate Logical. Whether tmean and cmd should be aggregated.
#'
#' @details
#'
#' CMD = Sum of Monthly CMD (difference between Eref and monthly
#' precipitation)
#'
#' Mean Temperature = Mean of monthly average temperature
#' (Midpoint: (Monthly maximum temp + Monthly minimum temp) / 2)
#'
#' @inheritParams common_docs
#' @returns Annual CMD raster tif and Annual Mean Temperature raster tif saved
#'   to an 'intermediate' folder in the `path_ccei`. If `aggregate == TRUE`,
#'   then a final raster with mean and standard deviations calculated.
#' @export
#'
#' @examples
#' \dontrun{
#'   ccei_values()
#' }

ccei_values <- function(rasts, out, aggregate = FALSE,
                        overwrite = TRUE, quiet = FALSE) {

  # Files
  out_cmd <- paste0(out, "_groups_cmd.tif")
  out_tmean <- paste0(out, "_groups_tmean.tif")
  out_final <- paste0(out, "_all_vars.tif")

  # Get boundaries of western hemisphere
  clip <- ccei_clip()

  groups <- unique(rasts$group)

  all <- purrr::walk(groups, function(g) {
    if(!quiet) rlang::inform(paste0(g, " - ", round(Sys.time())))
    # if(!quiet) rlang::inform(capture.output(lobstr::mem_used()))
    r <- filter(rasts, .data$group == .env$g)
    v <- ccei_vars(prec_files = r$file[r$var == "prec"],
                   tmax_files = r$file[r$var == "tmax"],
                   tmin_files = r$file[r$var == "tmin"],
                   clip, quiet)
    terra::writeRaster(v, paste0(out, "_", g, ".tif"), overwrite = overwrite)
  }, .progress = !quiet)

  #rlang::inform(capture.output(lobstr::mem_used()))

  f <- fs::dir_ls(fs::path_dir(out), regexp = paste0(groups, collapse = "|"))

  if(!quiet) rlang::inform("Combining and Saving rasters with annual data")

  # Combine all cmd layers
  terra::rast(f, lyrs = seq(1, length.out=length(f), by = 2)) %>%
    stats::setNames(paste0("cmd_", groups)) %>%
    terra::writeRaster(out_cmd, overwrite = overwrite)

  # Combine all tmean layers
  terra::rast(f, lyrs = seq(2, length.out=length(f), by = 2)) %>%
    stats::setNames(paste0("tmean_", groups)) %>%
    terra::writeRaster(out_tmean, overwrite = overwrite)

  # NOTE: for stdev() using default, pop = TRUE, to calculate Population SD
  if(aggregate) {
    # Calculate  final historical means and sd (interannual standard deviation)

    all_cmd <- terra::rast(out_cmd)
    all_tmean <- terra::rast(out_tmean)

    if(!quiet) rlang::inform("Final calculations")
    #rlang::inform(capture.output(lobstr::mem_used()))
    final <- c(
      stats::setNames(terra::mean(all_cmd), "cmd_mean"),
      stats::setNames(terra::stdev(all_cmd), "cmd_sd"),
      stats::setNames(terra::mean(all_tmean), "tmean_mean"),
      stats::setNames(terra::stdev(all_tmean), "tmean_sd")
    )

    terra::writeRaster(final, out_final, overwrite = overwrite)
  } else {

    c(terra::rast(out_cmd), terra::rast(out_tmean)) %>%
      terra::writeRaster(out_final, overwrite = overwrite)
  }
}

#' Calculate monthly Climate Moisture Deficit and Mean Temperature
#'
#' Because `climr:::calc_Eref()` and and `climr:::calc_CMD()` are expected to
#' work on vectors, they don't work with a raster in memory.
#'
#' `terra::app()` is very slow as it applies a function to every cell, where as
#' even if we extract the raster values to memory, being able to apply things in
#' parallel is much faster.
#'
#' Therefore we extract raster values calculate the Eref and CMD and then return
#' as a raster. It's faster if we omit the NAs for these calculations.
#'
#' @param prec_files Character. Files paths to precipitation rasters
#' @param tmin_files Character. Files paths to minimum temperature rasters
#' @param tmax_files  Character. Files paths to maximum temperature rasters
#' @param clip SpatExtent. Area which to clip the raster to.
#'
#' @inheritParams common_docs
#'
#' @returns SpatRaster with `tmean` and `cmd`

ccei_vars <- function(prec_files, tmin_files, tmax_files, clip, quiet = FALSE) {

  # Months stored as layers vs. files
  # - Crop first if layers, later if files
  if(length(prec_files) == 1) {
    get_rast <- function(x, m) x[[m]]
    if(!quiet) rlang::inform("  Cropping rasters first...")
    prec_files <- terra::rast(prec_files) %>% terra::crop(clip)
    tmax_files <- terra::rast(tmax_files) %>% terra::crop(clip)
    tmin_files <- terra::rast(tmin_files) %>% terra::crop(clip)
    sample <- prec_files
  } else {
    get_rast <- function(x, m) {
      terra::rast(x[m]) %>%
        terra::crop(clip)
    }
    sample <- terra::rast(prec_files[1], lyrs = 1) %>%
      terra::crop(clip)
  }

  # Prep infrastructure
  cells <- terra::ncell(sample)
  vals_cmd <- vals_tmean <- matrix(nrow = cells, ncol = 12)
  lat <- stats::setNames(terra::init(sample, "y"), "latitude") %>%
    terra::values(mat = FALSE) |>
    abs() # climr::calc_Eref() might treat negative latitudes incorrectly
          # - https://github.com/LandSciTech/ccviR/issues/209

  # Calculate eref, cmd, tmean
  # TODO: Use exported climr functions when available

  for(m in 1:12) {
    if(!quiet) rlang::inform(paste0("  Month: ", m))

    prec <- get_rast(prec_files, m) %>%
      terra::values(mat = FALSE)
    tmmax <- get_rast(tmax_files, m) %>%
        terra::values(mat = FALSE)
    tmmin <- get_rast(tmin_files, m) %>%
      terra::values(mat = FALSE)

    # Here, slightly faster to use non-na values only
    n <- !is.na(prec)
    eref <- climr::calc_Eref(
      m, tmmin = tmmin[n], tmmax = tmmax[n], latitude = lat[n])
    vals_cmd[n, m] <- climr::calc_CMD(eref, prec[n])
    vals_tmean[n, m] <- (tmmax[n] + tmmin[n])/2
  }

  # Here, much faster to use non-na values only
  if(!quiet) rlang::inform("  Calculate Annual values")
  r <- terra::rast(sample)
  a <- rep(NA_real_, cells)
  a[n] <- rowSums(vals_cmd[n, ])
  r[["cmd"]] <- a

  a <- rep(NA_real_, cells)
  a[n] <- rowMeans(vals_tmean[n, ])
  r[["tmean"]] <- a
  r
}


#' Bounding box to clip rasters to the Americas
#' @noRd
ccei_clip <- function() {
  rnaturalearth::ne_countries(continent = c("North America", "South America")) %>%
    sf::st_buffer(units::set_units(100, "km")) %>%
    terra::vect() %>%
    terra::ext()
}

#' Standardized Euclidean Distance for local climate
#'
#' From Williams et al. 2007, calculate the Standardized Euclidean Distance
#' (SED) for local climate change for a series of raster tiles.
#'
#' Each named list item represents a different climate variable. Names must be
#' consistent among `b`, `a`, and `s`. Each value represents the value for that
#' climate variable for a given raster tile.
#'
#' @param b Named List of Numeric Vectors. Vectors of values for Future climate variables.
#' @param a Named List of Numeric Vectors. Vectors of values for Historical climate variables.
#' @param s Named List of Numeric vectors. Each list item represents a vector for a
#'   different climate variable. Values are standard deviations of the
#'   interannual variability for historical climate variable.
#' @noRd
#' @examples
#'
#' calc_sed(b = list("cmd" = c(400, 300, 200), "tmean" = c(31, 30, 32)),
#'          a = list("cmd_mean" = c(45, 34, 35), "tmean_mean" = c(26, 25, 27)),
#'          s = list("cmd_sd" = c(30, 33, 29), "tmean_sd" = c(0.25, 0.26, 0.24)))

calc_sed <- function(b, a, s) {
  l <- list(b, a, s) %>%
    stats::setNames(c(".b", ".a", ".s")) %>%
    purrr::pmap(function(.b, .a, .s) (.b - .a)^2 / .s^2)

  sqrt(purrr::reduce(l, `+`))
}


#' Caculate CCEI from prepared rasters
#'
#' @inheritParams common_docs
#' @param models Character Vector. Subset of models to include (otherwise uses
#'   all models).
#' @param scenario Character. Which scenario to calculate CCEI for. Must match
#'   scenario used in raster file names, e.g., "ssp245" or "ssp585"
#' @param out_append Character. String to apped to the output file.
#'
#' @inheritParams common_docs
#'
#' @returns Writes final CCEI rasters (one for each scenario) to `path_ccei`.
#'   `ccei_SCENARIO.tif`.
#' @export
#'
#' @examples
#' \donttest{
#' calc_ccei(scenario = "ssp245")
#' calc_ccei(scenario = "ssp585")
#' calc_ccei(scenario = "ssp245",
#'           models = c("ACCESS-ESM1-5", "CanESM5"),
#'           out_append = "test")
#' }
calc_ccei <- function(path_ccei = "misc/ccei", scenario,
                      models = NULL, out_append = NULL,
                      overwrite = FALSE, quiet = FALSE) {

  m <- fs::dir_ls(fs::path(path_ccei, "intermediate"),
                  regexp = paste0("future(.+)", scenario))
  if(!is.null(models)) m <- stringr::str_subset(m, paste0(models, collapse = "|"))

  if(length(m) == 0) stop("No files detected for scenario: ", scenario, call. = FALSE)

  # Report models and scenarios used
  if(!quiet) {
    rlang::inform(c(
      paste0("Using models (n = ", dplyr::n_distinct(m), ") for ", scenario, ":"),
      stringr::str_remove_all(fs::path_file(m), "(future_)|(-ssp\\d{3}\\.tiff?)")))
  }

  hist <- terra::rast(fs::path(path_ccei, "intermediate", "hist_all_vars.tif")) %>%
    terra::values()

  # If Hist SD values == 0, replace with either 0.00001 or the smallest value
  # whichever is lowest. This prevent missing CCEI values caused by dividing by 0
  for(v in c("cmd_sd", "tmean_sd")) {
    zeros <- hist[, v] == 0
    hist[zeros, v] <- min(0.00001, min(hist[!zeros, v], na.rm = TRUE))
  }

  ccei <- purrr::map(m, ~{
    # Load future models individually, rather than using future_all_vars.tif for memory and
    # ease of calling columns
    future <- terra::rast(.x) %>%
      terra::values()

    b <- list(future[, "cmd"], future[, "tmean"])
    a <- list(hist[, "cmd_mean"], hist[, "tmean_mean"])
    s <- list(hist[, "cmd_sd"], hist[,"tmean_sd"])

    #r <- terra::rast(terra::rast(m[.x]))
    #r[["ccei"]] <- calc_sed(b, a, s)
    #r
    calc_sed(b, a, s)
  }, .progress = !quiet)


  c <- ccei
  c <- rlang::exec("cbind", !!!c) %>%
    rowMeans()

  r <- terra::rast(terra::rast(m[1]))
  r[[paste0("ccei_", scenario)]] <- c
  terra::writeRaster(r, fs::path(path_ccei, paste0(
    "ccei_", scenario,
    if(!is.null(out_append)) paste0("_", out_append),
    ".tif")), overwrite = overwrite)
}

#' Utility function to test if a tif or zip file has been fully downloaded
#'
#' Used by the `data-raw/data_ccei.R` workflow for downloading historical and
#' future CCEI data.
#'
#' @param files Character Vector. Files to check.
#'
#' @returns Logical vector same length as input. TRUE for downloaded, FALSE for
#' either not present, or not readable.
#'
#' @noRd

is_downloaded <- function(files) {
  purrr::map_lgl(files, ~ {
    tryCatch({
      if(file_exists(.x)) {
        if(fs::path_ext(.x) == "zip") {
          unzip(.x, list = TRUE)
        } else if(path_ext(.x) == "tif") {
          terra::rast(.x)
        }
        TRUE       # If exists and unzippable
      } else FALSE # If doesn't exist
    },
    # If not readable (download unfinished)
    warning = function(w) FALSE,
    error = function(e) FALSE)
  })
}



prep_out <- function(path, dir, type) {
  out_dir <- fs::path(path, dir)
  fs::dir_create(out_dir)
  out <- fs::path(out_dir, type)
  return(out)
}
