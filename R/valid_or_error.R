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

  if(any(st_geometry_type(poly) == "GEOMETRYCOLLECTION")){
    poly <- sf::st_collection_extract(poly, "POLYGON")
  }

  if(nrow(poly) > 1){
    if(nrow(poly) > 100){
      warning("The ", var_name, " povided contains more than 100 polygons. ",
              "These will be unioned to create one polygon. ",
              "If this is not expected please use a different shapefile.",
              call. = FALSE)
    }
    poly <- sf::st_union(poly) %>% sf::st_as_sf()
  }

  poly
}

