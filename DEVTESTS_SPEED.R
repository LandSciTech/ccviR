# Reprojections for CCEI ----------------------------------
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
ccei_classes

# Loading protected areas -----------------------------------

system.time(r1 <- sf::st_read("misc/protected_areas/pa_north_america.gpkg")) # ~ 1-1.5 seconds
system.time(r2 <- sf::st_read("misc/external_test_files/tundra_example/pa_north_america.shp")) # ~ 109s!!!!

# Cropping -----------------------------------------------

pth <- "misc/external_test_files/tundra_example"
hs_rcl <- rng_chg_mat(1, c(2, 6), 7)
clim <- get_clim_vars(fs::path(pth, "clim_prepared"),
                      scenario_names = c("RCP 4.5", "RCP 8.5"))
clim <- clim[c("mat", "cmd", "ccei", "clim_poly")]

system.time({
  analyze_spatial(
    range_poly = sf::read_sf(fs::path(pth, "BANS_NA.shp")),
    scale_poly = sf::read_sf(fs::path(pth, "CAN.shp")),
    clim_vars_lst = clim,
    #non_breed_poly = sf::read_sf(fs::path(pth, "BANS_non_breeding.shp")),
    #ptn_poly = sf::read_sf(fs::path(pth, "tundra_EPSG102008.shp")),
    hs_rast = terra::rast(fs::path(pth, "marshlands_AWPE_classifiedchange_WGS84.tif")),
    hs_rcl = hs_rcl,
    protected_poly = sf::read_sf("misc/protected_areas/pa_north_america_no_sm.gpkg"),
    gain_mod = 1,
    scenario_names = c("RCP 4.5", "RCP 8.5"))
})

system.time({
  analyze_spatial(
    range_poly = sf::read_sf(fs::path(pth, "BANS_NA.shp")),
    scale_poly = sf::read_sf(fs::path(pth, "CAN.shp")),
    clim_vars_lst = get_clim_vars(fs::path(pth, "clim_prepared"),
                                  scenario_names = c("RCP 4.5", "RCP 8.5")),
    non_breed_poly = sf::read_sf(fs::path(pth, "BANS_non_breeding.shp")),
    ptn_poly = sf::read_sf(fs::path(pth, "tundra_EPSG102008.shp")),
    hs_rast = terra::rast(fs::path(pth, "marshlands_AWPE_classifiedchange_WGS84.tif")),
    hs_rcl = matrix(c(-1, 0, 1, 1, 2, 3), ncol = 2),
    #protected_poly = sf::read_sf("misc/protected_areas/pa_north_america.gpkg"),
    protected_poly = sf::read_sf("misc/protected_areas/pa_canada.gpkg"),
    gain_mod = 1,
    scenario_names = c("RCP 4.5", "RCP 8.5"))
})

base_pth <- fs::path_package("ccviR", "extdata")
spat_res <- analyze_spatial(
  range_poly = sf::read_sf(file.path(base_pth, "rng_poly.shp"), agr = "constant"),
  scale_poly = sf::read_sf(file.path(base_pth, "assess_poly.shp"), agr = "constant"),
  protected_poly = sf::read_sf("misc/protected_areas/pa_north_america.gpkg"),
  clim_vars_lst = get_clim_vars(file.path(base_pth, "clim_files/processed"),
                                scenario_names = c("RCP 4.5", "RCP 8.5")),
  hs_rast = terra::rast(c(file.path(base_pth, "rng_chg_45.tif"),
                          file.path(base_pth, "rng_chg_85.tif"))),
  hs_rcl = matrix(c(-1, 0, 1, 1, 2, 3), ncol = 2),
  scenario_names = c("RCP 4.5", "RCP 8.5")
)



