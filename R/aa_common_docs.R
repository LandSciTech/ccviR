#' Documentation common to different functions
#'
#' @param path_ccei Character. Path to pre-downloaded climate data to be used to
#'   calculate CCEI. Expects two folders `historical` and `future`.
#' @param overwrite Logical. Whether to overwrite existing file(s)
#' @param quiet Logical. Whether to suppress progress messages.
#' @param scenario_name Character. A string identifying the climate change
#'   scenario(s) that will be used as a suffix for the output files.
#' @param out_folder Character. Path where the processed files will be saved.
#' @param brks List. Optionally supply `brks_mat`, `brks_cmd` and `brks_ccei`
#'   as a list.
#' @param brks_mat,brks_cmd,brks_ccei a matrix containing breaks to use for
#'   classifying mat, cmd and ccei into 6, 6 and 4 classes, respectively. See
#'   \code{\link[raster]{reclassify}} for details on the matrix format. If NULL,
#'   the default, the breaks will be determined using the median and half the
#'   interquartile range
#'
#' @details
#' Use `@inheritParams common_docs` to include the above in any function
#' documentation with a matching argument (will only include matching args)
#'
#' @keywords internal
#' @name common_docs
NULL
