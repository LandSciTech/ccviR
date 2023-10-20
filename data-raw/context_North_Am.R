## code to prepare `context_North_Am` dataset goes here

NAm <- rnaturalearth::ne_states(c("united states of america", "canada"),
                                returnclass = "sf")

tmap::qtm(NAm, fill = NA, borders = "black")

context_North_Am <- NAm %>% select(name)
usethis::use_data(context_North_Am, overwrite = TRUE)

