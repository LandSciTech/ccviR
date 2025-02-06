# if not valid try making it valid and produce an error if not

validate_poly <- function(poly, var_name) {
  if(!all(sf::st_is_valid(poly))){
    poly <- sf::st_make_valid(poly)

    validate(need(
      all(sf::st_is_valid(poly)),
      paste0(var_name, " is not valid. Check the polygon is ",
             "correct or provide a different version.")
    ))
  }
  poly
}


valid_or_error <- function(poly, var_name = "Spatial data") {

  poly <- validate_poly(poly, var_name)

  if(any(st_geometry_type(poly) == "GEOMETRYCOLLECTION")){
    poly <- sf::st_collection_extract(poly, "POLYGON")
  }

  # TODO: Catch this message in the Shiny app
  if(nrow(poly) > 1) {
    if(nrow(poly) > 100){
      warning("The ", var_name, " povided contains more than 100 polygons. ",
              "These will be unioned to create one polygon. ",
              "If this is not expected please use a different shapefile.",
              call. = FALSE)
    }
    poly <- sf::st_union(poly) %>% sf::st_as_sf()

    validate_poly(poly, var_name = paste0("Unionized ", var_name))
  }

  poly
}

