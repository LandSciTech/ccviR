#' Make Vulnerability Table
#'
#' Make an empty vulnerability factor table that can be filled in with the
#' appropriate value for each vulnerability factor. The Code corresponds to the
#' questions in the NatureServe spreadsheet. See the NatureServe Guidelines for
#' detailed instructions. Values can be -1: Unknown, 0: Neutral, 1: Somewhat
#' Increase, 2: Increase, 3: Greatly Increase.
#'
#' @param sp_nm Species name
#' @param val1 A single number to fill the first column with. The default -1 for
#'   Unknown should be used in most cases.
#' @param val2,val3,val4 Additional values. Use default NA
#' @param cave 0 or 1 For whether the species is cave or ground water dependent.
#'   See Guidelines.
#' @param mig 0 or 1 is the species migratory?
#'
#' @return a data.frame that can be edited and used as input for \code{vuln_df}
#'   in \code{\link{calc_vulnerability}}
#'
#' @export
#'
#' @examples
#' make_vuln_df("sfa", cave = 1, mig = 0)

make_vuln_df <- function(sp_nm, val1 = -1, val2 = NA, val3 = NA, val4 = NA,
                         cave = 0 , mig = 0){
  vuln_qs <- tibble::tribble(
    ~Species, ~Code,
    sp_nm, "Z2",
    sp_nm, "Z3",
    sp_nm, "B1",
    sp_nm, "B2a",
    sp_nm, "B2b",
    sp_nm, "B3",
    sp_nm, "C1",
    sp_nm, "C2ai",
    sp_nm, "C2aii",
    sp_nm, "C2bi",
    sp_nm, "C2bii",
    sp_nm, "C2c",
    sp_nm, "C2d",
    sp_nm, "C3",
    sp_nm, "C4a",
    sp_nm, "C4b",
    sp_nm, "C4c",
    sp_nm, "C4d",
    sp_nm, "C4e",
    sp_nm, "C4f",
    sp_nm, "C4g",
    sp_nm, "C5a",
    sp_nm, "C5b",
    sp_nm, "C5c",
    sp_nm, "C6",
    sp_nm, "D1",
    sp_nm, "D2",
    sp_nm, "D3",
    sp_nm, "D4",
  ) %>% mutate(Value1 = val1, Value2 = val2,
                     Value3 = val3, Value4 = val4)
  vuln_qs$Value1[1] <- cave
  vuln_qs$Value1[2] <- mig
  vuln_qs
}

