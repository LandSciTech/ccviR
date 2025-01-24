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
ca_rast <- fasterize::fasterize(ca1, r, field = "IUCN_CAT", fun = "min")
ca_rast <- terra::rast(ca_rast)
terra::plot(ca_rast)
terra::writeRaster(ca_rast, path(dir_pa, "pa_canada_raster.tif"))


# No transformation - For checking
r <- raster::raster(raster::extent(ca), res = 1000, crs = terra::crs(ca))
ca_rast_check <- fasterize::fasterize(ca, r, field = "IUCN_CAT", fun = "min")
ca_rast_check <- terra::rast(ca_rast_check)
terra::plot(ca_rast_check)

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
us_rast <- fasterize(us1, r, field = "IUCN_Cat", fun = "min") # Take smallest value of IUCN_Cat
us_rast <- terra::rast(us_rast)

terra::plot(us_rast)
terra::writeRaster(us_rast, path(dir_pa, "pa_usa_raster.tif"))

# No transformation - For checking
r <- raster::raster(raster::extent(us), res = 1000, crs = raster::crs(us))
us_rast_check <- fasterize(us, r, field = "GAP_Sts", fun = "min")
us_rast_check <- terra::rast(us_rast_check)
terra::plot(us_rast_check)


# Combine ---------------------------------------------------------------------
ca_rast <- terra::rast(path(dir_pa, "pa_canada_raster.tif"))
us_rast <- terra::rast(path(dir_pa, "pa_usa_raster.tif"))
north_america <- terra::merge(ca_rast, us_rast)
terra::writeRaster(north_america, path(dir_pa, "pa_north_america.tif"))

terra::plot(north_america)
terra::plot(us_rast)


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




International Union for the Conservation of Nature (IUCN) management categories assigned to protected areas for inclusion in the United Nations Environment World Conservation Monitoring Center (UNEP-WCMC) World Database for Protected Areas (WDPA), the North America Intergovernmental Committee on Cooperation for Wilderness and Protected Areas Conservation (NAWPA) Protected Area Database, and the Commission for Environmental Cooperation (CEC) North American Terrestrial Protected Areas Database. IUCN defines a protected area as, "A clearly defined geographical space, recognized, dedicated and managed, through legal or other effective means, to achieve the long-term conservation of nature with associated ecosystem services and cultural values" (includes GAP Status Code 1 and 2 only). Management categories are not hierarchical and follows as:

  'IUCN Category Ia': Strict Nature Reserves are strictly protected areas set aside to protect biodiversity and possibly geological or geomorphological features, where human visitation, use and impacts are strictly controlled and limited to ensure preservation of the conservation values. Such protected areas can serve as indispensable reference areas for scientific research and monitoring.

'IUCN Category Ib': Wilderness Areas protected areas are usually large unmodified or slightly modified areas, retaining their natural character and influence, without permanent or significant human habitation, which are protected and managed to preserve their natural condition.

'IUCN Category II': National Park protected areas are large natural or near natural areas set aside to protect large-scale ecological processes, along with the complement of species and ecosystems characteristic of the area, which also provide a foundation for environmentally and culturally compatible spiritual, scientific, educational, recreational and visitor opportunities.

'IUCN Category III': Natural Monument or Feature protected areas are set aside to protect a specific natural monument, which can be a landform, sea mount, submarine caverns, geological features such as caves, or even a living feature such as an ancient grove. They are generally quite small protected areas and often have high visitor value.

'IUCN Category IV': Habitat and (or) species management protected areas aim to protect particular species or habitats and management reflects this priority. Many Category IV protected areas will need regular, active interventions to address the requirements of particular species or to maintain habitats, but this is not a requirement of this category.

'IUCN Category V': Protected landscape and (or) seascape protected areas occur where the interaction of people and nature over time has produced an area of distinct character with significant ecological, biological, cultural, and scenic value.

'IUCN Category VI': Protected area with sustainable use (community based, non-industrial) of natural resources are generally large, with much of the area in a more-or-less natural condition and whereas a proportion is under sustainable natural resource management and where such exploitation is seen as one of the main aims of the area.

'Other Conservation Areas' are not recognized by IUCN at this time; however, they will be evaluated to determine if they meet the definition of Other Effective Area Based Conservation Measures (OECMs) for inclusion in the WDPA following recently released guidance.

These areas (GAP Status Code 3 areas only) are attributed in the 'IUCN Category' Domain along with 'Unassigned' areas (GAP Status Code 4). In addition, a few areas are included as 'Not Reported', these areas meet the definition of IUCN protection (i.e. GAP Status Code 1 or 2) but 'IUCN Category' has not yet been assigned and categorical assignment is not appropriate. See the PAD-US Data Manual, Table 12, for a crosswalk from Designation Type, GAP Status Code, and size to IUCN Category.

