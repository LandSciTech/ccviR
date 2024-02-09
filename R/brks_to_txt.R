#' Convert breaks matrix to text that can be stored in readme
#'
#' @param brks a matrix of breaks with columns start, end and class
#'
#' @return A string with breaks in the format "class: (start - end);"
#' @export
#'
#' @examples
#' brks_to_txt(matrix(data = 1:6, nrow = 2, byrow = TRUE))
brks_to_txt <- function(brks){
  if(is.null(brks)){
    return("")
  }
  paste0(brks[,3],": (",round(brks[,1], 2), " - ", round(brks[,2], 2), ")", collapse = ";")
}

