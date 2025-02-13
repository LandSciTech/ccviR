#https://open.canada.ca/data/en/dataset/6c343726-1e92-451a-876a-76e17d398a1c

#https://www.cec.org/north-american-environmental-atlas/north-american-protected-areas-2021/

# Setup -----------------------------------------------------

library(httr2)
library(fs)
library(glue)
library(sf)
library(dplyr)
library(fasterize) # For quick rasterization (see references at the bottom)

# Setup file paths
dir_pa <- path("misc", "protected_areas")
dir_CAN <- path(dir_pa, "Canada")
dir_USA <- path(dir_pa, "USA")
dir_create(c(dir_pa, dir_CAN, dir_USA))

# Resolution (for lat/lon rasters)
# 1 degree = 60 arc-minutes
# 2.5 min = ~21 km2 at Equator
# 30 s = ~1 km2 at Equator
res <- 0.01   # ~ 1.11 km2

# Download data ----------------------------------------------------------------

## Canadian Government ----------------------
# https://open.canada.ca/data/en/dataset/6c343726-1e92-451a-876a-76e17d398a1c
f_CAN <- path(dir_CAN, "ProtectedConservedArea_2023.zip")
if(!file_exists(f_CAN)) {
  request("https://data-donnees.az.ec.gc.ca/api/file?path=%2Fspecies%2Fprotectrestore%2Fcanadian-protected-conserved-areas-database%2FDatabases%2FProtectedConservedArea_2023.zip") %>%
    req_progress() %>%
    req_perform(path = f_CAN)
  unzip(f_CAN, exdir = dir_CAN)
}

## USA Government ----------------------------------
# https://www.usgs.gov/programs/gap-analysis-project/science/pad-us-data-download
# https://www.sciencebase.gov/catalog/item/652ef930d34edd15305a9b03
f_USA <- path(dir_USA, "PADUS4_0Geodatabase.zip")
if(!file_exists(f_USA)) {
  # May need to get link by hand every time from sciencebase URL (above)
  request("https://prod-is-usgs-sb-prod-content.s3.amazonaws.com/652ef930d34edd15305a9b03/PADUS4_0Geodatabase.zip?AWSAccessKeyId=AKIAI7K4IX6D4QLARINA&Expires=1737735173&Signature=W2jrMRgpldQAtOcBvCWKTZxcybA%3D") %>%
    req_progress() %>%
    req_perform(path = f_USA)
  unzip(f_USA, exdir = dir_USA)
}

## Optional -----------------------
# 2021 Joint data from the CEC for Canada, USA, Mexico
# https://www.cec.org/north-american-environmental-atlas
# https://www.cec.org/north-american-environmental-atlas/north-american-protected-areas-2021/

if(FALSE) {
  request("http://www.cec.org/files/atlas_layers/1_terrestrial_ecosystems/1_02_protected_areas_2021/protectedareas_2021_mappackage.zip") %>%
    req_progress() %>%
    req_perform(path = path(dir_pa, "protectedareas_2021_mappackage.zip"))
}


# Rasterize ----------------------------------
#
# In both cases there are "MUTLISURFACE" Geometries which seeme to be collections
# of MULTIPOLYGONS and POLYGONS. These need to be converted to a standard format
# first. cf https://github.com/r-spatial/sf/issues/748.

## Canada ---------------------------
# Data manual: https://data-donnees.az.ec.gc.ca/api/file?path=%2Fspecies%2Fprotectrestore%2Fcanadian-protected-conserved-areas-database%2FUserManuals%2FProtectedConservedArea_UserManual_2023.pdf

f_CAN <- path(dir_CAN, "ProtectedConservedArea_2023", "ProtectedConservedArea_2023.gdb")
st_layers(f_CAN)
ca <- st_read(f_CAN, layer = "ProtectedConservedArea_2023") %>%
  st_cast("MULTIPOLYGON") %>% # Convert "MULTISURFACE" geometries into MULTIPOLYGONS
  select("ZONE_ID",     # ID
         "STATUS",      # 0 - Delisted
         "PA_OECM_DF",  # 1 Protected Area; 2 OECM; 3 Interim PA; 4 Interim OECM; 5 NA
         "IUCN_CAT",    # 1-7 are IUCN Recognized Protected areas, 8 & 9 are unreported or other
         "TYPE_E",      # Description of the type of protected area
         "ZONEDESC_E",  # Description of the zone
         "OWNER_E") %>% # Owner
  filter(IUCN_CAT <= 7)  # Using only IUCN recognized (i.e. I - VI, 1-7)

# Verify that they are all 'standard'
sf::st_geometry_type(ca) |> unique()

ca1 <- st_transform(ca, crs = 4326)

r <- raster::raster(raster::extent(ca1), res = res, crs = 4326)
#ca_rast <- fasterize::fasterize(ca1, r, field = "IUCN_CAT", fun = "min") # Keep smallest value of IUCN_Cat
ca_rast <- fasterize::fasterize(ca1, r, field = "IUCN_CAT", fun = "any") # Keep Yes/No protected
ca_rast <- terra::rast(ca_rast)
#terra::plot(ca_rast)
terra::writeRaster(ca_rast, path(dir_pa, "pa_canada_raster.tif"), overwrite = TRUE)


# No transformation - For checking
if(FALSE) {
  r <- raster::raster(raster::extent(ca), res = 1000, crs = terra::crs(ca))
  ca_rast_check <- fasterize::fasterize(ca, r, field = "IUCN_CAT", fun = "min")
  ca_rast_check <- terra::rast(ca_rast_check)
  terra::plot(ca_rast_check)
}

## USA ------------------------------
# Use st_wrap_dateline() deal with polygons near the date line
# https://stackoverflow.com/a/55164190
# OGR SQL - https://gdal.org/en/stable/user/ogr_sql_dialect.html

f_USA <- path(dir_USA, "PADUS4_0_Geodatabase.gdb")
st_layers(f_USA)

# This can take a couple of minutes
us <- st_read(
  f_USA,
  query = paste("SELECT *",
                "FROM PADUS4_0Combined_Proclamation_Marine_Fee_Designation_Easement",
                #"FROM PADUS4_0Marine", # For a quick look
                # Using only IUCN recognized
                "WHERE IUCN_Cat IN ('Ia','Ib', 'II', 'III', 'IV', 'V', 'VI')")) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(IUCN_Cat = as.numeric(factor(
    IUCN_Cat, levels = c("Ia", "Ib", "II", "III", "IV", "V", "VI"))))

# Verify that they are all 'standard'
sf::st_geometry_type(us) |> unique()

# Transform to 4326 before rasterizing
us1 <- us %>%
  st_transform(crs = 4326) %>%
  st_wrap_dateline() # Required to fix (split) Polygons near date-line

r <- raster::raster(raster::extent(us1), res = res, crs = 4326)
#us_rast <- fasterize(us1, r, field = "IUCN_Cat", fun = "min") # Keep smallest value of IUCN_Cat
us_rast <- fasterize(us1, r, field = "IUCN_Cat", fun = "any") # Keep Yes/No protected
us_rast <- terra::rast(us_rast)

#terra::plot(us_rast)
terra::writeRaster(us_rast, path(dir_pa, "pa_usa_raster.tif"), overwrite = TRUE)

# No transformation - For checking
if(FALSE) {
  r <- raster::raster(raster::extent(us), res = 1000, crs = raster::crs(us))
  us_rast_check <- fasterize(us, r, field = "GAP_Sts", fun = "min")
  us_rast_check <- terra::rast(us_rast_check)
  terra::plot(us_rast_check)
}

# Combine ---------------------------------------------------------------------
# Use Virtual merge to avoid memory issues
# https://github.com/rspatial/terra/issues/210#issuecomment-841723729
north_america <- terra::vrt(c(path(dir_pa, "pa_canada_raster.tif"),
                              path(dir_pa, "pa_usa_raster.tif")),
                            path(dir_pa, "pa_north_america.vrt"), overwrite = TRUE)

terra::writeRaster(north_america,
                   path(dir_pa, "pa_north_america.tif"),
                   overwrite = TRUE)

#terra::plot(north_america)


# Citations -------------------------------------------------------------------

# U.S. Geological Survey (USGS) Gap Analysis Project (GAP), 2024, Protected Areas Database of the United States (PAD-US) 4.0: U.S. Geological Survey data release, https://doi.org/10.5066/P96WBCHS.

# References ------------------------------------------------------------------

# USA PAD Data Manual - https://www.usgs.gov/programs/gap-analysis-project/pad-us-data-manual
# USA PAD Data Dictionary - https://d9-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/media/files/PADUS_Standard_Tables_1.xlsx
#
# The 'GAP Status Code' (GAP_Sts):
#
# 'GAP Status Code 1': An area having permanent protection from conversion of natural land cover and a mandated management plan in operation to maintain a natural state within which disturbance events (of natural type, frequency, intensity, and legacy) are permitted to proceed without interference or are mimicked through management.
#
# 'GAP Status Code 2': An area having permanent protection from conversion of natural land cover and a mandated management plan in operation to maintain a primarily natural state, but which may receive uses or management practices that degrade the quality of existing natural communities, including suppression of natural disturbance.
#
# 'GAP Status Code 3': An area having permanent protection from conversion of natural land cover for most of the area, but subject to extractive uses of either a broad, low-intensity type (e.g., logging, Off Highway Vehicle recreation) or localized intense type (e.g., mining). It also confers protection to Federally listed endangered and threatened species throughout the area.
#
# 'GAP Status Code 4': There are no known public or private institutional mandates or legally recognized easements or deed restrictions held by the managing entity to prevent conversion of natural habitat types to anthropogenic habitat types. The area generally allows conversion to unnatural land cover throughout or management intent is unknown. See the PAD-US Standards Manual GAP Status Code Assignment reference document for a summary of assumptions, criteria, and methods or the geodatabase 'GAP_Status' lookup table for short descriptions.



# fasterize
# https://ecohealthalliance.github.io/fasterize/
# https://gis.stackexchange.com/questions/284018/processing-vector-to-raster-faster-with-r-on-windows
