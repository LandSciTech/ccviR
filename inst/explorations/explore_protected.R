library(sf)
library(fasterize)
library(dplyr)
library(fs)

# This assumes you have the data from data-raw/data_protected.R downloaded into ./misc/

# Canadian ------------------------------------------------------
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
         "OWNER_E")     # Owner

# Checks
ca_df <- st_drop_geometry(ca)

filter(ca_df, STATUS == 0) # No delisted sites included

count(ca_df, IUCN_CAT)     # Lots in the 8/9 category (Other or NA)
count(ca_df, IUCN_CAT < 8) # Still most in IUCN recognized categories
count(ca_df, IUCN_CAT < 8, PA_OECM_DF) # Only PA_OECM 1 and 3 are in IUCN valid cats

ca_df %>%
  filter(IUCN_CAT %in% c(8,9)) %>%
  count(PA_OECM_DF, TYPE_E)

# Alberta Provincial parsk which aren't recognized as being protected...
ca_df %>%
  filter(PA_OECM_DF == 5, TYPE_E == "Wildland Provincial Park")

# Not IUCN but includes properties by Ducks Unlimited etc.
ca_df %>%
  filter(PA_OECM_DF == 3, TYPE_E == "Ownership by Environmental Non-Governmental Organization")

# Verify that they are all 'standard'
sf::st_geometry_type(ca) %>% unique()

r <- raster::raster(ca, res = 1000)
ca_rast <- fasterize::fasterize(ca, r, field = "STATUS", fun = "first")
ca_rast <- terra::rast(ca_rast)
terra::plot(ca_rast)

# USA
f_USA <- path(dir_USA, "PADUS4_0_Geodatabase.gdb")
st_layers(f_USA)
