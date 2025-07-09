## code to prepare `context_North_Am` dataset goes here
north_am <- rnaturalearth::ne_states(c("united states of america", "canada"),
                                     returnclass = "sf") %>%
  select(name)

context_North_Am <- sf::st_simplify(north_am, dTolerance = 1000)

plot(north_am[1,])
plot(context_North_Am[1,])

usethis::use_data(context_North_Am, overwrite = TRUE)

