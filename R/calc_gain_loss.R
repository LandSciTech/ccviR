# Get the proportion of habitat gained and lost in range
#' Get the proportion of habitat gained and lost in range
#'
#' @param rast
#' @param poly
#' @param eer_pkg
#'
#' @return
#' @export
#'
#' @examples
calc_gain_loss <- function(rast, poly,
                           eer_pkg){
  if(eer_pkg){
    withCallingHandlers(
      warning = function(cnd){
        if(grepl("transformed to raster", conditionMessage(cnd))){
          message("Polygons were transformed to have CRS matching raster")
          invokeRestart("muffleWarning")
        }
      },
      out <- exactextractr::exact_extract(rast, poly, progress = FALSE)
    )

    out <- out[[1]]

  } else {
    out <- raster::extract(rast, poly,  df = TRUE) %>%
      purrr::set_names("ID", "value")

    out <- out %>% mutate(coverage_fraction = 1)
  }

  out <- out %>% group_by(value) %>%
    summarise(coverage_fraction = sum(coverage_fraction)) %>%
    filter(!is.na(value), value > 0) %>%
    mutate(gain_loss = case_when(value == 1 ~ "lost",
                                 value > 1 & value < 7 ~ "maint",
                                 value == 7 ~ "gain") %>%
             factor(levels = c("gain", "lost", "maint"))) %>%
    group_by(gain_loss, .drop = FALSE) %>%
    summarise(coverage_fraction = sum(coverage_fraction)) %>%
    tidyr::pivot_wider(names_from = gain_loss, values_from = coverage_fraction) %>%
    transmute(perc_lost = (lost/(lost + maint) * 100) %>% round(3),
              perc_gain = (gain/(lost + maint) * 100) %>% round(3),
              perc_maint = (maint/(lost+maint) * 100) %>% round(3))

  return(out)
}
