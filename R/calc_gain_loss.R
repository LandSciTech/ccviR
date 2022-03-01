#' Calculate changes in the range in the assessment area
#'
#' range_change is (loss - gain)/(loss + maintained) which is change in range
#' relative to current size of range
#'
#' range_overlap is (maintained)/(loss + maintained) which is percent of the
#' current range represented by an intersection of the predicted future and
#' current ranges
#'
#' @param rast
#' @param poly
#'
#' @noRd
calc_gain_loss <- function(rast, poly, gain_mod){

  out <- calc_prop_raster(rast, poly, var_name = "HS", val_range = 0:3,
                          digits = 10) %>%
    transmute(range_change = ((HS_1 - HS_3 * gain_mod)/(HS_1 + HS_2) * 100) %>% round(3),
              range_overlap = (HS_2/(HS_1 + HS_2) * 100) %>% round(3))

  return(out)
}
