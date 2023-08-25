
#' Convert breaks matrix to text that can be stored in readme

brks_to_txt <- function(brks){
  if(is.null(brks)){
    return("")
  }
  paste0(brks[,3],": (",round(brks[,1], 2), " - ", round(brks[,2], 2), ")", collapse = ";")
}

