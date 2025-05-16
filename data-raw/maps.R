## code to prepare `context_North_Am` dataset goes here
north_am <- rnaturalearth::ne_states(c("united states of america", "canada"),
                                     returnclass = "sf") %>%
  select(name)

context_North_Am <- sf::st_simplify(north_am, dTolerance = 1000)

plot(north_am[1,])
plot(context_North_Am[1,])

usethis::use_data(context_North_Am, overwrite = TRUE)


non_breed0 <- rnaturalearth::ne_states("united states of america", returnclass = "sf") %>%
  filter(name == "Florida") %>%
  select(name)

non_breed <- sf::st_simplify(non_breed0, dTolerance = 1000)

plot(non_breed0[1,])
plot(non_breed[1,])

usethis::use_data(non_breed, overwrite = TRUE)
sf::st_write(non_breed, fs::path(fs::path_package("ccviR", "extdata"), "non_breed.shp"))
