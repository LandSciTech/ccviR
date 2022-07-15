#' Make vulnerability table
#'
#' Make an empty vulnerability factor table that can be filled in with the
#' appropriate value for each vulnerability factor. The Code corresponds to the
#' questions in the NatureServe Guidelines and Question is an abbreviated
#' version of the question. See the NatureServe Guidelines for detailed
#' instructions on how to score species. Values can be -1: Unknown, 0: Neutral,
#' 1: Somewhat Increase, 2: Increase, 3: Greatly Increase.
#'
#' @param sp_nm Species name
#' @param val1 A single number to fill the first column with. The default -1 for
#'   Unknown should be used in most cases.
#' @param val2,val3,val4 Additional values. Use default NA
#' @param cave 0 or 1 For whether the species is cave or ground water dependent.
#'   See Guidelines.
#' @param mig 0 or 1 is the species migratory?
#' @param use_spatial if TRUE then values for factors that are calculated in
#'   \code{analyze_spatial} will be set to -1 for Unknown so that they do not
#'   override the results of the spatial analysis
#'
#' @return a data.frame that can be edited and used as input for \code{vuln_df}
#'   in \code{\link{calc_vulnerability}}
#'
#' @export
#'
#' @examples
#' make_vuln_df("sfa", cave = 1, mig = 0)

make_vuln_df <- function(sp_nm, val1 = -1, val2 = NA, val3 = NA, val4 = NA,
                         cave = 0, mig = 0, use_spatial = TRUE){

  cave_mig <- data.frame(Species = sp_nm, Code = c("Z2", "Z3"),
                         Question = c("Is the species an obligate of caves or groundwater systems",
                                      "Is the species migratory"),
                         Value1 = c(cave, mig))


  vuln_qs <- vulnq_code_lu_tbl %>%
    mutate(Species = sp_nm,
           Value1 = val1, Value2 = val2, Value3 = val3, Value4 = val4) %>%
    mutate(across(matches("Value\\d"), ~ifelse(!is.na(is_spatial) & is_spatial == 1 & use_spatial & !is.na(.x),
                           -1, .x)))
  vuln_qs <- bind_rows(cave_mig, vuln_qs) %>%
    select(!matches("Value\\d"), everything())


  return(vuln_qs)
}

