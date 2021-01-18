## GIS ANALYSIS FOR NATURESERVE CCVI ##
# Created on: January 11th 2021 by Sarah Endicott #

# these packages are needed for some functions but don't need to be attached
if(!requireNamespace("exactextractr", quietly = TRUE)){
  install.packages("exactextractr")
}

if(!requireNamespace("gdalUtils", quietly = TRUE)){
  install.packages("gdalUtils")
}

# I tend to load raster first if I am using tidyverse due to naming conflicts
library(raster)
library(tidyverse)
library(sf)
# library(rasterVis)
# library(tmap)

# this will load the functions defined in this script into Global environment
# so that we can use them below
source("scripts/CCVI_functions.R")

# faster project raster
ref_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
rast_pth <- "data/species_files/AWPE/marshlands_AWPE_amwpel_breeding_2055_45_ENSEMBLE_classifiedchange.tif"

out <- wrap_gdalwarp(rast_pth, ref_crs$input, "data/marshlands_AWPE_WGS84.tif",
                     overwrite = TRUE, output_Raster = TRUE)

folder_loc <- "data/hs_files"

fls <- list.files(folder_loc, full.names = TRUE)

map(fls, ~wrap_gdalwarp(.x, ref_crs, str_replace(.x, "change", "changeWGS84")))

# # compare results
# input <- raster(rast_pth)
#
# plot(input)
# plot(out)

# Run CCVI for one species
AMCO_NA <- run_CCVI_calcs(
  species_nm = "AMCO",
  scale_nm = "NA",
  root_pth = "data"
)

 # Run CCVI for one species
BANS_NA <- run_CCVI_calcs(
  species_nm = "BANS",
  scale_nm = "NA",
  root_pth = "data"
)

non_breed_BANS <- st_read("data/species_files/BANS/BANS_non_breeding.shp")
un_non_breed_BANS <- st_union(non_breed_BANS)
# time for 1 run is 0.72 mins
# hs_rast is 165 million cells which slows things down. Either fix the issue
# with things wraping around the 180/-180 line or reduce resolution?

# Run for multiple scales
AMCO_all <- map_df(list("NA", "CAN", "USA"), ~run_CCVI_calcs("AMCO", .x, "data"))

write.csv(AMCO_all, "data/outputs/AMCO_NA_CAN_USA.csv", row.names = FALSE)

# Run for multiple species and scales
results_all <- map_df(list("AMCO", "AWPE"),
                      ~map_df(list("NA", "CAN", "USA"),
                              ~run_CCVI_calcs(.y, .x, "data"), .y = .x))

write.csv(results_all, "data/outputs/AMCO_AWPE_NA_CAN_USA.csv", row.names = FALSE)



# Tried raster::extract version but takes > 20 min so stopped
# source("scripts/CCVI_functions.R")
# library(raster)
# library(tidyverse)
# library(sf)
# AWPE_NA <- run_CCVI_calcs(
#   species_nm = "AWPE",
#   scale_nm = "NA",
#   root_pth = "data",
#   force_crs = FALSE,
#   eer_pkg = FALSE
# )
