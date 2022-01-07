# if not valid try making it valid and produce an error if not

valid_or_error <- function(poly, var_name){
  if(!all(sf::st_is_valid(poly))){
    poly <- sf::st_make_valid(poly)

    if(!all(sf::st_is_valid(poly))){
      stop("The ", var_name, " is not valid. Check the polygon is ",
           "correct or provide a different version",
           call. = FALSE)
    }
  }
  poly
}

