# Create Sea-level Rise raster
#
# Download and combine data to create SLR rasters for North America.
#
# Data Sources:
# - CanCoast 2.0 https://ostrnrcan-dostrncan.canada.ca/entities/publication/caff0f1b-6adb-4470-be2c-d4461cf29793
# - NOAA https://oceanservice.noaa.gov/hazards/sealevelrise/
# - NASA https://sealevel.nasa.gov/ipcc-ar6-sea-level-projection-tool

# NOAA shows the extent that sea level rise would result in inundations
# for various sea level rise scenarios (i.e. 0-10ft),
#
# CanCoast shows *which* shorelines will experience how much sea level rise in the
# next 100 years, but it isn't scenario specific, and also doesn't show how much
# inundation this would result in.
#
# NatureServe:
# > Exposure to Sea Level Rise
# > NOTES: This factor comes into play only in the case that all or a portion of the range within the
# > assessment area may be subject to the effects of a 0.5-1 m or greater sea level rise and the
# > consequent influence of storm surges and intrusion of salt water. Most climate model scenarios
# > predict at least a 0.5 m sea level rise. Because projected sea level rise (0.5-2 m by 2100) is great
# > compared to historical sea level changes, the negative impact on habitats for most affected
# > species is expected to be high.
#
# So I take this to mean that we need a raster which shows the level of
# inundation at >=0.5m sea level rise, and then we take the proportion of the
# assessment area that overlaps this raster.
#
# We can get this information from NOAA, but not from CanCoast
#
# However, there will be very low levels of overlap as coastal areas and low lying
# spots are generally reasonably narrow.
# Is there a better way of scoring this or is this mostly applicable to
# shore birds and other species that exist in these narrow strips of land?
#
# - https://coast.noaa.gov/digitalcoast/tools/slr.html
#
# - https://open.canada.ca/data/en/dataset/18752265-bda3-498c-a4ba-9dfe68cb98da/resource/e3c69512-74d2-4ce0-af67-df3df8dddfbc
# - https://ftp.maps.canada.ca/pub/elevation/dem_mne/MRDEM_MNEMR/CanElevation-MRDEM-Product-Specifications.pd\
# - https://download-telecharger.services.geo.ca/pub/elevation/dem_mne/MRDEM_MNEMR/CanElevation-MRDEM-Usage-Guide.pdf
# https://download-telecharger.services.geo.ca/pub/elevation/dem_mne/MRDEM_MNEMR/CanElevation-MRDEM-Usage-Guide.pdf#%5B%7B%22num%22%3A69%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C69%2C720%2C0%5D


library(httr2)
library(fs)
library(glue)
library(sf)

dir_slr <- path("misc", "slr")
dir_CAN <- path(dir_slr, "Canada")
dir_USA <- path(dir_slr, "USA")
dir_create(c(dir_slr, dir_CAN, dir_USA))

# Downloads -------------------------------------

request("https://cmssostrsharedprod.z9.web.core.windows.net/gid_314669.zip") %>%
  req_progress() %>%
  req_perform(path = path(dir_CAN, "gid_314669.zip"))

unzip(path(dir_CAN, "gid_314669.zip"), exdir = dir_CAN, overwrite = TRUE)

# https://coast.noaa.gov/slrdata/Sea_Level_Rise_Vectors/OR/index.html
request("https://chs.coast.noaa.gov/htdata/Inundation/SLR/BulkDownload/Sea_Level_Rise_Vectors/OR/OR_MFR_slr_data_dist.zip") %>%
  req_progress() %>%
  req_perform(path = path(dir_USA, "OR_MFR_slr_data_dist.zip"))

unzip(path(dir_USA, "OR_MFR_slr_data_dist.zip"), exdir = dir_USA, overwrite = TRUE)



can <- st_read(path(
  dir_CAN,
  "CANCOAST_CSI_V2_5_6/CANCOAST_CSI_V2_5_6.shp"))

can <- st_read(path(
  dir_CAN,
  "CANCOAST_SEALEVELCHANGE_2006_2099_V1",
  "CANCOAST_SEALEVELCHANGE_2006_2099_V1.shp"))

nb <- rnaturalearth::ne_states(country = "Canada") %>%
  filter(name == "New Brunswick") %>%
  st_transform(st_crs(can))

nb_slr <- st_crop(can, st_bbox(nb))

library(ggplot2)
ggplot(data = nb) +
  geom_sf() +
  geom_sf(data = nb_slr, aes(colour = SLChange))
plot(nb_slr[1])

st_layers(path(dir_USA, "OR_MFR_slr_final_dist.gdb"))
or_slr <- st_read(path(dir_USA, "OR_MFR_slr_final_dist.gdb"), layer = "OR_MFR_low_0ft")
or_slr2 <- st_read(path(dir_USA, "OR_MFR_slr_final_dist.gdb"), layer = "OR_MFR_slr_6ft") %>%
  mutate(depth = 6) |>
  bind_rows(
    st_read(path(dir_USA, "OR_MFR_slr_final_dist.gdb"), layer = "OR_MFR_slr_0ft") %>%
      mutate(depth = 0)
  ) %>%
  mutate(depth = factor(depth, levels = c(6, 0)))

or <- rnaturalearth::ne_states(country = "United States of America") %>%
  filter(name == "Oregon") %>%
  st_transform(st_crs(or_slr))

ggplot(data = st_crop(or, st_bbox(or_slr2))) +
  #geom_sf(fill = "green") +
  geom_sf(data = filter(or_slr2, depth == 6), aes(fill = depth)) #+
  #geom_sf(data = or_slr, colour = "red")


#https://download-telecharger.services.geo.ca/pub/elevation/dem_mne/MRDEM_MNEMR/CanElevation-MRDEM-Usage-Guide.pdf

t <- terra::rast("https://datacube-prod-data-public.s3.ca-central-1.amazonaws.com/store/elevation/mrdem/mrdem-30/mrdem-30-dtm.tif")


t <- terra::rast("https://datacube-prod-data-public.s3.ca-central-1.amazonaws.com/store/elevation/mrdem/mrdem-30/mrdem-30-dtm.vrt")

nb <- rnaturalearth::ne_states(country = "Canada") %>%
  filter(name == "New Brunswick") %>%
  sf::st_transform(sf::st_crs(t)) %>%
  terra::ext()


t1 <- terra::crop(t, terra::ext(nb))
terra::plot(t1)


t1 <- terra::crop(t, terra::ext(1774874, 1818832, -89162, -52305))
terra::plot(t1)


t1 <- terra::crop(t, nb)
terra::plot(t1)
t0 <- t1
t0[t0 > 1] <- NA
ts <- t0
ts[ts > 0] <- NA
terra::plot(t0, col = "red", add = TRUE)
terra::plot(ts, col = "blue", add = TRUE)


# https://cds.climate.copernicus.eu/requests?tab=all

unzip(path(dir_slr, "69987d7db64653547af811835c4fe588.zip"),
      overwrite = TRUE,exdir = dir_slr)
library(stars)
slr <- read_ncdf(path(dir_slr, "future_tide_absolute-change_2021-2050_MHHW_v1.nc"), make_time = FALSE)

read_stars(path(dir_slr, "future_tide_absolute-change_2021-2050_MHHW_v1.nc"))
read_stars(path(dir_slr, "future_tide_absolute-change_2021-2050_MHHW_v1.nc"))
read_stars(path(dir_slr, "future_tide_absolute-change_2021-2050_MSL_v1.nc"))
t <- read_stars(path(dir_slr, "future_waterlevel_absolute-change_2021-2050_ensemble-counts-negative_50-percentile_v1.nc"),
           driver = NULL, sub = FALSE)

x = read_stars(path(dir_slr, "future_waterlevel_absolute-change_2021-2050_ensemble-counts-negative_50-percentile_v1.nc"), curvilinear = c("PRODUCT/SUPPORT_DATA/GEOLOCATIONS/longitude_bounds", "PRODUCT/SUPPORT_DATA/GEOLOCATIONS/latitude_bounds"))

t <- ncdf4::nc_open(path(dir_slr, "future_tide_absolute-change_2021-2050_MSL_v1.nc"))
t <- stars::st_as_stars(t)

longitude <- t$var[[1]]$vals
latitude <- t$dim[[3]]$vals



# Citation ---------------------------------------
# Manson, G.K., Couture, N.J., and James, T.S., 2019. CanCoast Version 2.0: data and indices to describe the sensitivity of Canada's marine coasts to changing climate; Geological Survey of Canada, Open File 8551, 1 .zip file. https://doi.org/10.4095/314669
