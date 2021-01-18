## GIS ANALYSIS FOR NATURESERVE CCVI ##
# Created on: January 7th 2021 by Sarah Ouimette #

# Generally avoid rm(list = ls)) because it can cause trouble for some one else
# using your script

# Set working directory and load packages ---------------------------------
library(tidyverse)
library(raster)
library(sf)
library(rasterVis)
library(tmap)
library(rgeos)
library(sp)
library(rgdal)

# this will load the functions defined in this script into Global environment
# so that we can use them below
source("scripts/CCVI_functions.R")

# It is generally bad practice to use setwd because it can cause confusion for
# people running your code. It is better to use relative paths. RStudio
# automatically makes the project folder your working directory so you can write
# paths relative to that, and then to save repetition save the path to the data
# folder from the project and use paste to read in files

base_pth <- "data/Files_for_GIS_Analysis_for_CCVI/"
# Load Species Range Data (polygons) -------------------------------------------------

# American Coot #
AMCO_NA <- st_read(paste0(base_pth, "AMCO_NA.shp"))
AMCO_CAN <- st_read(paste0(base_pth, "AMCO_CAN.shp"))
AMCO_USA <- st_read(paste0(base_pth, "AMCO_USA.shp"))

#American White Pelican #
AWPE_NA  <- st_read(paste0(base_pth, "AWPE_NA.shp"))
AWPE_CAN  <- st_read(paste0(base_pth, "AWPE_CAN.shp"))
AWPE_USA  <- st_read(paste0(base_pth, "AWPE_USA.shp"))

# Section A - Exposure to Local Climate Change: Temperature ---------------
# Load temperature rasters #
temp_NA <- raster(paste0(base_pth, "MAT_delta_100_reclass_NA.tif"))
temp_CAN <- raster(paste0(base_pth, "MAT_delta_100_reclass_CAN.tif"))
temp_USA <- raster(paste0(base_pth, "MAT_delta_100_reclass_USA.tif"))

# Extract temperature raster values to polygon of species range to determine percentage of species range found in each raster category (1-6) #

AMCO_MAT <- pmap_df(list(list(AMCO_CAN, AMCO_USA, AMCO_NA), 
                         list(temp_CAN, temp_USA, temp_NA),
                         list("CAN", "USA", "NA")), 
                    ~calc_prop_raster(..2, ..1, "MAT", ..3))

# Output = csv with species, scale and the percentage of species' range found in categories 1-6 #

# Section A - Exposure to Local Climate Change: Moisture ------------------
# Load moisture rasters (data has been reprojected in QGIS using GDAL warp (reproject) with nearest neighbor) #
moist_NA <- raster(paste0(base_pth, "CMD_delta_reclass_NA_WGS84.tif"))
moist_CAN <- raster(paste0(base_pth, "CMD_delta_reclass_CAN_WGS84.tif"))
moist_USA <- raster(paste0(base_pth, "CMD_delta_reclass_USA_WGS84.tif"))
crs(moist_NA)

# Ignore lines 64-69 for now CMD has been reprojected in QGIS 
# #Moisture raster needs to be reprojected into WGS84 #
# newproj <-  "+proj=longlat +datum=WGS84 +no_defs"
# pr1 <- projectRaster(moist_NA, crs=newproj) # takes too long
# ## much faster when using GDAL:
# rp_moist_NA <-rgdal::reproject(moist_NA,crs="newproj", program = "GDAL")
# could not find function reproject

# Extract moisture raster values to polygon of species range to determine
# percentage of species range found in each raster category (1-6) #

AMCO_CMD <- pmap_df(list(list(AMCO_CAN, AMCO_USA, AMCO_NA), 
                         list(moist_CAN, moist_USA, moist_NA),
                         list("CAN", "USA", "NA")), 
                    ~calc_prop_raster(..2, ..1, "CMD", ..3))

# Output = csv with species, scale and the percentage of species' range found in categories 1-6 #

# Section A - Exposure to Local Climate Change: Migratory Exposure --------
# Load species non-breeding range #
AMCO_non_breeding <- st_read (paste0(base_pth, "AMCO_non_breeding.shp"))
AWPE_non_breeding  <- st_read(paste0(base_pth, "AWPE_non_breeding.shp"))
# Load Climate Change Exposure Index Raster #
ccei <- raster(paste0(base_pth, "ccei_reclass_CRS.tif"))

# Extract ccei raster values to polygon of species non-breeding range to determine percentage of species range found in each raster category (1-4) #
AMCO_CCEI <- calc_prop_raster(ccei, AMCO_non_breeding, "CCEI", "Non-breeding")

# Output = csv with species and the percentage of species' range found in categories 1-4 #

# Ideally temperature, moisture, and migratory exposure would all be output into one CSV# 

# Section C - Sensitivity and Adaptive Capacity: Historical Thermal niche --------
histtemp <- raster(paste0(base_pth, "MWMT_MCMT_100_reclass.tif"))

AMCO_HTN_NA <- calc_prop_raster(histtemp, AMCO_NA, "HTN", "NA")


# Section C - Sensitivity and Adaptive Capacity: Physiological Thermal niche --------
# Percentage of species range at each scale (NA, CAN, USA)overlapping with the tundra #
# Load polygon of the tundra #
tundra <- st_read(paste0(base_pth, "tundra.shp"))

# here all files first would need to be reprojected into equal area projection** # 
AMCO_PTN <- list(AMCO_CAN, AMCO_USA, AMCO_NA) %>% 
  map(~st_transform(.x, st_crs(tundra))) %>% 
  map2_df(list("CAN", "USA", "NA"), ~calc_overlap_poly(.x, tundra, "PTN", .y))

# Section C - Sensitivity and Adaptive Capacity: Historical Hydrological --------
# range (maximum - minimum) of mean annual precipitation

# ONLY run once to reproject 
# temp_NA <- raster::raster(paste0("data/Files_for_GIS_Analysis_for_CCVI/",
#                                  "MAT_delta_100_reclass_NA.tif"))
# 
# histhydro <- raster::raster(paste0("data/Files_for_GIS_Analysis_for_CCVI/", 
#                                    "MAP.asc"))
# 
# histhydro_proj <- raster::projectRaster(histhydro, crs = raster::crs(temp_NA), 
#                                         filename = paste0("data/Files_for_GIS_Analysis_for_CCVI/",
#                                                           "MAP_WGS84.tif"))

histhydro <- raster(paste0("data/Files_for_GIS_Analysis_for_CCVI/",
                    "MAP_WGS84.tif"))

range_MAP <- calc_min_max_raster(histhydro, AMCO_NA, "MAP")

# Section D - Modelled Response to Climate Change -------------------------
# Load Polygons of North America, Canada, and the USA #
NAM <- st_read(paste0(base_pth, "NORTH_AMERICA.shp"))
CAN <- st_read(paste0(base_pth, "CANADA.shp"))
USA <- st_read(paste0(base_pth, "UNITED_STATES.shp")) 

#Load Habitat Suitability Models for Each Species #

# ONLY run once to reproject
# temp_NA <- raster::raster(paste0("data/Files_for_GIS_Analysis_for_CCVI/",
#                          "MAT_delta_100_reclass_NA.tif"))
# 
# 
# AMCO_hs <- raster::raster(paste0("data/Files_for_GIS_Analysis_for_CCVI/",
#                          "marshlands_AMCO_y00475_breeding_2055_45_ENSEMBLE_classifiedchange.tif"))
# 
# # need to remove some NA rows at the top to make projection work, also cuts off
# # tip of Ellesmere Island
# AMCO_hs_crop <- raster::crop(AMCO_hs, raster::extent(AMCO_hs)*c(1, 1, 1, 0.75))
# 
# # Use ngb for categorical variable
# AMCO_hs_proj <- raster::projectRaster(AMCO_hs_crop, crs = raster::crs(temp_NA),
#                                       method = "ngb", overwrite = TRUE,
#                                       filename = paste0("data/Files_for_GIS_Analysis_for_CCVI/",
#                                                         "marshlands_AMCO_y00475_breeding_2055_45_ENSEMBLE_classifiedchange_WGS84.tif"))

AMCO_hs_proj <- raster(paste0(base_pth,
                         "marshlands_AMCO_y00475_breeding_2055_45_ENSEMBLE_classifiedchange_WGS84.tif"))

# For calculating percentages this is one is a bit different...
# Percentage range lost = category 1 / total of 1-6
#Percentage range maintained = sum of categories 2-6 / total of categories 1-6
# Percentage range gained = category 7 / total of categories 1-6

AMCO_mod_res <- calc_gain_loss(AMCO_hs_proj, CAN)


# Range Size --------------------------------------------------------------
# Calculate the area of each species range at each scale 
# Reproject data into Equal Area Projection 
range_area <- st_area(AMCO_NA)


# Percentage Range in Canada ----------------------------------------------
# Calculate the percentage of each species' North American range that overlaps
# with Canada

range_CAN <- calc_overlap_poly(AMCO_NA, CAN, "perc_overlap_CAN")



