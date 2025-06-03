# Notes ----------------------------

## Simplifications ------------------------------------------
# Use st_simplify vs. rmapshaper::ms_simplify()
# - We don't require the preserved matching among polygon border that
#   mapshaper uses and dTolerance of st_simplify is simpler to understand.
# - use preserveTopology = TRUE in `st_simplify()` to keep small islands
# - Use dTolerance of 50m (amount of simplification - related to distance between vertices to be removed and edges)

# References
# - https://r.geocompx.org/geometry-operations.html?q=simplify#simplification
# - https://pro.arcgis.com/en/pro-app/latest/tool-reference/cartography/simplify-polygon.htm

# Other notes
# - Transform CRS after simplification because starting CRS are appropriate
# - Use ESRI:102008 which is a North American Equal Albers projection (https://epsg.io/102008)


# Setup -----------------------------------------------------

library(httr2)
library(fs)
library(glue)
library(sf)
library(dplyr)
library(units)

# Setup file paths
dir_pa <- path("misc", "protected_areas")
dir_CAN <- path(dir_pa, "Canada")
dir_USA <- path(dir_pa, "USA")
dir_create(c(dir_pa, dir_CAN, dir_USA))

# Coordinate reference systems - Preserve area
crs_out <- "ESRI:102008"

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
# Use Vector Analysis and Summary Statistics data set (not 'Full Inventory')
# https://www.usgs.gov/programs/gap-analysis-project/science/pad-us-data-download
# https://www.sciencebase.gov/catalog/item/652d4ebbd34e44db0e2ee458

f_USA <- path(dir_USA, "PADUS4_0VectorAnalysis_GAP_PADUS_Only_ClipCENSUS.zip")
if(!file_exists(f_USA)) {
  # May need to get link by hand every time from sciencebase URL (above)
  request("https://prod-is-usgs-sb-prod-content.s3.us-west-2.amazonaws.com/652d4ebbd34e44db0e2ee458/PADUS4_0VectorAnalysis_GAP_PADUS_Only_ClipCENSUS.zip?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20250327T161204Z&X-Amz-SignedHeaders=host&X-Amz-Expires=86399&X-Amz-Credential=AKIAI7K4IX6D4QLARINA%2F20250327%2Fus-west-2%2Fs3%2Faws4_request&X-Amz-Signature=bc4591d127825bdab761b4f0a85b3fa271b9d55a21e7e09434eaccac4c2f5f9a") %>%
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


# Simplify ----------------------------------
#
# In both cases there are "MUTLISURFACE" Geometries which seeme to be collections
# of MULTIPOLYGONS and POLYGONS. These need to be converted to a standard format
# first. cf https://github.com/r-spatial/sf/issues/748.

## Canada ---------------------------
# Data manual: https://data-donnees.az.ec.gc.ca/api/file?path=%2Fspecies%2Fprotectrestore%2Fcanadian-protected-conserved-areas-database%2FUserManuals%2FProtectedConservedArea_UserManual_2023.pdf

### Setup ----

f_CAN <- path(dir_CAN, "ProtectedConservedArea_2023", "ProtectedConservedArea_2023.gdb")
#st_layers(f_CAN)

ca <- st_read(f_CAN, layer = "ProtectedConservedArea_2023") %>%
  st_cast("MULTIPOLYGON") %>% # Convert "MULTISURFACE" geometries into MULTIPOLYGONS
  select(
    "ZONE_ID",     # ID
    "IUCN_CAT",    # 1-7 are IUCN Recognized Protected areas, 8 & 9 are unreported or other
    "BIOME",       # Type of protected area (M - Fully or partially marine;
                   #   T - Terrestrial or Freshwater"
    #"STATUS",      # 0 - Delisted
    #"PA_OECM_DF",  # 1 Protected Area; 2 OECM; 3 Interim PA; 4 Interim OECM; 5 NA

    #"TYPE_E",      # Description of the type of protected area
    "ZONEDESC_E",  # Description of the zone
    #"OWNER_E"      # Owner
    ) %>%
  filter(IUCN_CAT <= 7, # Using only IUCN recognized (i.e. I - VI, 1-7)
         BIOME != "M")  # Omit Marine Biomes

# Verify that they are all 'standard' (i.e. MULTIPOLYGONs)
sf::st_geometry_type(ca) %>% unique()

### Simplify and union -----------------------------

# Look at the sizes of small features (could also first separate into polygons)
areas <- st_area(ca)
min(areas)
summary(areas)
length(areas)

# Set our cutoff to 50m x 50m (to correspond to simplification of 50m)
cut <- set_units(2500, "m2")

# This cutoff results in:

# Removal of 0.000015% area (0.18 km2 out of 1,221,842 km2)
(t <- set_units(sum(areas), "km2"))
(tt <- set_units(sum(areas[areas >= cut]), "km2"))

(t - tt)
(t - tt) / t * 100

# Removal of 1.4% features (175 out of 12,703)
(l <- length(areas))
(ll <- length(areas[areas >= cut]))

(l - ll)
(l - ll) / l * 100

ca1 <- ca[areas >= cut, ] %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 50) %>%
  st_make_valid()%>%
  st_union() %>%
  st_transform(crs = crs_out) %>%
  st_make_valid()

all(st_is_valid(ca1))

p <- path(dir_pa, "pa_canada_no_sm.gpkg")
unlink(p)
st_write(ca1, p, overwrite = TRUE, append = FALSE)


### Explore - Final set of polygons ------------------------------
if(FALSE) {
  library(ggplot2)
  library(patchwork)

  # Plot full set of polygons
  ggplot() +
    geom_sf(data = ca1)

  # How do areas compare before and after simplification?
  # - Union original data to make it comparable
  # - 99.97% of area preserved
  sum(st_area(ca1)) / sum(st_area(st_union(ca))) * 100 # SLOW
}

### Explore - Simplification ------------------------------
if(FALSE) {
  library(ggplot2)
  library(patchwork)

  sm <- st_bbox(c(xmin = -67.65, xmax = -67.6,
                  ymin = 47.95, ymax = 48.05),
                crs = 4326)

  ca_test <- st_crop(ca, st_transform(sm, st_crs(ca)))

  ca_test_simp0 <- ca_test %>%
    st_simplify(preserveTopology = TRUE, dTolerance = 0) %>%
    st_union()

  ca_test_simp1 <- ca_test %>%
      st_simplify(preserveTopology = TRUE, dTolerance = 50) %>%
      st_union()

  ca_test_simp2 <- ca_test %>%
    st_simplify(preserveTopology = TRUE, dTolerance = 150) %>%
    st_union()


  # 99% of original area in simplified
  st_area(ca_test_simp0) / sum(st_area(ca_test)) * 100 # 99.5 %
  st_area(ca_test_simp1) / sum(st_area(ca_test)) * 100 # 99.3 %
  st_area(ca_test_simp2) / sum(st_area(ca_test)) * 100 # 98.7 %

  # Object sizes
  lobstr::obj_size(ca_test)       # 63.44 kB
  lobstr::obj_size(ca_test_simp0) # 58.06 kB
  lobstr::obj_size(ca_test_simp1) # 12.48 kB
  lobstr::obj_size(ca_test_simp2) #  9.16 kB


  g0 <- ggplot() +
    geom_sf(data = ca_test, fill = "yellow") +
    labs(title = "No simplification") +
    ggspatial::annotation_scale()

  g1 <- ggplot() +
    geom_sf(data = ca_test_simp1, fill = "red") +
    labs(title = "st_simplify, tolerance 0m") +
    ggspatial::annotation_scale()

  g2 <- ggplot() +
    geom_sf(data = ca_test_simp1, fill = "red") +
    labs(title = "st_simplify, tolerance 50m") +
    ggspatial::annotation_scale()

  g3 <- ggplot() +
    geom_sf(data = ca_test_simp1, fill = "red") +
    labs(title = "st_simplify, tolerance 150m") +
    ggspatial::annotation_scale()

  g0 + g1 + g2 + g3
}

### Compare simplifications ------------------------------
# mapshaper (via rmapshaper) vs st_simplify
# Some more details: https://datascience.blog.wzb.eu/2021/03/15/simplifying-geospatial-features-in-r-with-sf-and-rmapshaper/
if(FALSE) {
  library(rmapshaper) # For fast, topologically-aware simplifications
  library(ggplot2)
  library(patchwork)
  library(ggspatial)

  # Check that you have the system mapshaper installed
  # (if not: https://andyteucher.ca/rmapshaper/articles/rmapshaper.html#using-the-system-mapshaper)
  # Need to use system mapshaper because R can't handle these large files
  check_sys_mapshaper()

  sm <- st_bbox(c(xmin = -67.65, xmax = -67.6,
                  ymin = 47.95, ymax = 48.05),
                crs = 4326)

  ca_test <- st_crop(ca, st_transform(sm, st_crs(ca)))

  # Sf faster than
  bench::mark(sf = {
    ca_test_simp1 <- ca_test %>%
      st_simplify(preserveTopology = TRUE, dTolerance = 50) %>%
      st_union()
  }, rmsp = {
    ca_test_simp2 <- ca_test %>%
      ms_simplify(keep = 0.4, sys = TRUE, snap_interval = 50) %>%
      st_union()
  }, check = FALSE)

  # 92% of original area in simplified
  st_area(ca_test_simp1) / sum(st_area(ca_test)) * 100
  st_area(ca_test_simp2) / sum(st_area(ca_test)) * 100

  lobstr::obj_size(ca_test_simp1)
  lobstr::obj_size(ca_test_simp2)

  g0 <- ggplot() +
    geom_sf(data = ca_test, fill = "yellow") +
    labs(title = "No simplification") +
    annotation_scale()

  g1 <- ggplot() +
    geom_sf(data = ca_test_simp1, fill = "red") +
    labs(title = "sf Simplified") +
    annotation_scale()

  g2 <- ggplot() +
    geom_sf(data = ca_test_simp2, fill = "red") +
    labs(title = "mapshaper Simplified") +
    annotation_scale()

  g0 + g1 + g2
}


## USA ------------------------------
# Use st_wrap_dateline() deal with polygons near the date line
# https://stackoverflow.com/a/55164190
# OGR SQL - https://gdal.org/en/stable/user/ogr_sql_dialect.html

### Setup ----

f_USA <- path(dir_USA, "PADUS4_0VectorAnalysis_GAP_PADUS_Only_ClipCENSUS.gdb/")
#st_layers(f_USA)
# us_glimpse <- st_read(
#   f_USA,
#   query = paste(
#     "SELECT *",
#     "FROM PADUS4_0VectorAnalysis_GAP_Simp_SingP_ClipCENSUS",
#     "LIMIT 10"))

# This can take a couple of minutes
us <- st_read(
  f_USA,
  query = paste(
    "SELECT *",
    "FROM PADUS4_0VectorAnalysis_GAP_Simp_SingP_ClipCENSUS",
    # Using only IUCN recognized
    "WHERE IUCN_Cat IN ('Ia','Ib', 'II', 'III', 'IV', 'V', 'VI')")) %>%
  st_cast("MULTIPOLYGON") %>% # Deal with warnings about collections
  mutate(IUCN_Cat = as.numeric(factor(
    IUCN_Cat, levels = c("Ia", "Ib", "II", "III", "IV", "V", "VI")))) %>%
  # Also omit "fee" Features which are identified as Marine by name
  filter(!stringr::str_detect(tolower(Unit_Nm), "marine"))

# Note: What about Maritime and Marine parks?
# - "Alaska Maritime National Wildlife Refuge" -> Marine?
# filter(us, stringr::str_detect(tolower(Unit_Nm), "maritime|marine")) %>%
#   st_drop_geometry() %>%
#   select(Unit_Nm) %>%
#   distinct()

# Verify that they are all 'standard' (i.e. MULTIPOLYGONs)
sf::st_geometry_type(us) %>% unique()

### Simplify and union -----------------------------

# Look at the sizes of small features (could also first separate into polygons)
areas <- st_area(us)
min(areas)
summary(areas)
length(areas)

# Set our cutoff to 50m x 50m (to correspond to simplification of 50m)
cut <- set_units(2500, "m2")

# This cutoff results in:

# Removal of 0.00457% area (58 km2 out of 1,268,923 km2)
(t <- set_units(sum(areas), "km2"))
(tt <- set_units(sum(areas[areas >= cut]), "km2"))

(t - tt)
(t - tt) / t * 100

# Removal of 42% features (130,780 out of 314,069)
(l <- length(areas))
(ll <- length(areas[areas >= cut]))

(l - ll)
(l - ll) / l * 100

# Be patient!
us1 <- us[areas >= cut, ] %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 50) %>%
  st_make_valid() %>%
  st_union() %>%
  st_transform(crs = crs_out) %>%
  st_make_valid()

all(st_is_valid(us1))

p <- path(dir_pa, "pa_usa_no_sm.gpkg")
unlink(p)
st_write(us1, p, overwrite = TRUE, append = FALSE)

### Explore - Final set of polygons ------------------------------
if(FALSE) {
  library(ggplot2)
  library(patchwork)

  # Plot full set of polygons
  ggplot() +
    geom_sf(data = us1)

  # How do areas compare before and after simplification?
  # - Union original data to make it comparable
  # - This takes a long time, so use a smaller region
  med <- st_bbox(c(xmin = -100.75, xmax = -80.5,
                   ymin = 30, ymax = 40),
                 crs = 4326)
  us0 <- us %>%
    st_crop(st_transform(med, st_crs(us))) %>%
    st_make_valid() %>%  #st_buffer(0) %>%
    st_union()

  us1.1 <- st_buffer(us1, 0) %>%
    st_crop(st_transform(med, st_crs(us1))) %>%
    st_make_valid() %>% #st_buffer(0) %>%
    st_union()

  sum(st_area(us1.1)) / sum(st_area(us0)) * 100
}

### Explore - Simplification ------------------------------
if(FALSE) {
  library(ggplot2)
  library(patchwork)

  # Look at a small area
  sm <- st_bbox(c(xmin = -90.75, xmax = -90.5,
                  ymin = 35, ymax = 35.2),
                crs = 4326)

  us_test <- st_crop(us, st_transform(sm, st_crs(us)))

  us_test_simp0 <- us_test %>%
    st_simplify(preserveTopology = TRUE, dTolerance = 0) %>%
    st_union()

  us_test_simp1 <- us_test %>%
    st_simplify(preserveTopology = TRUE, dTolerance = 50) %>%
    st_union()

  us_test_simp2 <- us_test %>%
    st_simplify(preserveTopology = TRUE, dTolerance = 150) %>%
    st_union()


  # 99% of original area in simplified
  st_area(us_test_simp0) / sum(st_area(us_test)) * 100 # 100 %
  st_area(us_test_simp1) / sum(st_area(us_test)) * 100 # 99.6 %
  st_area(us_test_simp2) / sum(st_area(us_test)) * 100 # 99.3 %

  # Object sizes
  lobstr::obj_size(us_test)       # 40.61 kB
  lobstr::obj_size(us_test_simp0) # 27.44 kB
  lobstr::obj_size(us_test_simp1) # 19.70 kB
  lobstr::obj_size(us_test_simp2) # 17.55 kB


  g0 <- ggplot() +
    geom_sf(data = us_test, fill = "yellow") +
    labs(title = "No simplification") +
    ggspatial::annotation_scale()

  g1 <- ggplot() +
    geom_sf(data = us_test_simp0, fill = "red") +
    labs(title = "st_simplify, tolerance 0m") +
    ggspatial::annotation_scale()

  g2 <- ggplot() +
    geom_sf(data = us_test_simp1, fill = "red") +
    labs(title = "st_simplify, tolerance 50m") +
    ggspatial::annotation_scale()

  g3 <- ggplot() +
    geom_sf(data = us_test_simp2, fill = "red") +
    labs(title = "st_simplify, tolerance 150m") +
    ggspatial::annotation_scale()

  g0 + g1 + g2 + g3
}


# Combine ---------------------------------------------------------------------
# Be PATIENT!
ca1 <- st_read(path(dir_pa, "pa_canada_no_sm2.gpkg"))
us1 <- st_read(path(dir_pa, "pa_usa_no_sm2.gpkg"))

north_america <- rbind(ca1, us1)

all(st_is_valid(north_america))

p <- path(dir_pa, "pa_north_america_no_sm.gpkg")
unlink(p)
st_write(north_america,
         p,
         overwrite = TRUE, append = FALSE)

# Transforming Problems ----------------------------------
#plot(north_america)
na <- st_read(path(dir_pa, "pa_north_america_no_sm.gpkg"))

na_t <- st_transform(na, 4326) %>%
  st_make_valid()  # (SLOOOOOOOOOW)

any(!st_is_valid(na_t)) # So we have problems

# Where are the problems?
p <- st_cast(na_t, "POLYGON")
pp <- p[!st_is_valid(p), ]

nrow(p)
nrow(pp)  # 9 problems overall

pp$id <- seq_len(nrow(pp))

plot(pp)

plot(pp[1,])
plot(pp[2,])
plot(pp[3,])
plot(pp[4,])
plot(pp[5,])
plot(pp[6,])
plot(pp[7,])
plot(pp[8,])
plot(pp[9,])

ggplot() +
  geom_sf(data = context_North_Am) +
  geom_sf(data = pp[1,], fill = "blue") +
  coord_sf(xlim = c(-170, -80), ylim = c(40, 70))

# Something funky with the rivers?
ggplot() +
  geom_sf(data = context_North_Am) +
  geom_sf(data = pp[1,], fill = "blue") +
  coord_sf(xlim = c(-165, -148), ylim = c(64, 69))


# README ---------------------------------------------------------
# see data-raw/README_protected.md


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
