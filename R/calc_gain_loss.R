#' Calculate changes in the range in the assessment area
#'
#' range_change is (loss - gain)/(loss + maintained) which is change in range
#' relative to current size of range
#'
#' range_overlap is (maintained)/(loss + maintained) which is percent of the
#' current range represented by an intersection of the predicted future and
#' current ranges
#'
#' @param rast raster classified so 1 is lost, 2 maintained and 3 gained. Can
#'   also have 0 is not suitable
#' @param poly area within which the proportions lost, gained and maintained
#'   are calculated
#'
#' @noRd
calc_gain_loss <- function(rast, poly, gain_mod){

  out <- calc_prop_raster(rast, poly, var_name = "HS", val_range = 0:3,
                          digits = 10) %>%
    transmute(range_change = ((.data$HS_1 - .data$HS_3 * gain_mod)/(.data$HS_1 + .data$HS_2) * 100) %>% round(3),
              range_overlap = (.data$HS_2/(.data$HS_1 + .data$HS_2) * 100) %>% round(3))

  return(out)
}
