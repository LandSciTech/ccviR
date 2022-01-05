# Get the proportion of habitat gained and lost in range
#' Get the proportion of habitat gained and lost in range
#'
#' @param rast
#' @param poly
#'
#' @return
#' @export
#'
#' @examples
calc_gain_loss <- function(rast, poly, var_name){

  out <- calc_prop_raster(rast, poly, var_name = "HS", val_range = 0:3,
                          digits = 10) %>%
    transmute(perc_lost = (HS_1/(HS_1 + HS_2) * 100) %>% round(3),
              perc_gain = (HS_3/(HS_1 + HS_2) * 100) %>% round(3),
              perc_maint = (HS_2/(HS_1 + HS_2) * 100) %>% round(3))

  return(out)
}
