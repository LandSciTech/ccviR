# Testing speed of reprojections for CCEI
library(terra)
library(sf)
library(rnaturalearth)
library(bench)

non_breed_poly <- ne_countries(country = "United States of America") %>%
  st_transform("ESRI:102008") # Good area projection for North America

ccei <- rast(c("misc/ccei_processed/CCEI_reclassRCP_4.5.tif",
               "misc/ccei_processed/CCEI_reclassRCP_8.5.tif"))

plot(non_breed_poly)
plot(ccei)

# Not the same
same.crs(non_breed_poly, ccei)

# Clip CCEI to non-breeding area, then re-project ~3s for the USA

bench::mark({
  non_breed_clip <- st_transform(non_breed_poly, crs = sf::st_crs(ccei))
  ccei_clip <- terra::crop(ccei, non_breed_clip)
  ccei_clip <- terra::project(ccei_clip, terra::crs(non_breed_poly), method = "near")
})

devtools::load_all()
ccei_classes <- calc_prop_raster(ccei_clip, non_breed_poly, "CCEI",
                                 val_range = 1:4, check_overlap = 0,
                                 return_overlap_as = "prop_non_breed_over_ccei")
