## code to prepare `climate_data_pkg` to store climate data on OSF to be

library(purrr)

# Download the climate data from adaptwest
dat_pth <- "D:/Ilona/Climate_data/data/"

out_folder <- file.path(dat_pth, "ccviR_data_pkg/CMIP5_ENSEMBLE_rcp45_rcp85_2080_NORM_6190")

if(!dir.exists(out_folder)){
  dir.create(out_folder)
}


# # GADM is too detailed but does have sable island
# clim_poly <- sf::st_as_sf(raster::getData(country = c("CAN", "USA", "MEX"), level = 0))
#
# # world does not have sable island
# NAm <- geodata::world(path = ".") %>% st_as_sf() %>% filter(GID_0 %in% c("CAN", "USA", "MEX"))
#
# # get sable island
# plot(clim_poly %>% st_as_sf() %>% st_geometry())
# ext <- raster::drawExtent()
#
# sable <- st_crop(clim_poly %>% st_as_sf(), ext)
#
# NAm2 <- bind_rows(NAm, sable)
#
# write_sf(NAm2, "../Climate_data/data/North_Am_w_sable.shp")

# location of file with area that climate data is from in this case NA
clim_poly <- file.path(dat_pth, "North_Am_w_sable.shp")

# location of folders with future climate data names will become scenario names
fut_clim <- list(`RCP 4.5` = file.path(dat_pth, "NA_ENSEMBLE_rcp45_2080s_Bioclim_ASCII"),
                 `RCP 8.5` = file.path(dat_pth, "NA_ENSEMBLE_rcp85_2080s_Bioclim_ASCII"))

# location of folder with climate normals data
cur_clim <- file.path(dat_pth, "NA_NORM_6190_Bioclim_ASCII")

file_nms <- c("MAT", "CMD", "MAP", "MWMT", "MCMT")

# make readme csv
write.csv(
  data.frame(Scenario_Name = names(fut_clim),
             GCM_or_Ensemble_name = "AdaptWest 15 CMIP5 AOGCM Ensemble",
             Historical_normal_period = "1961-1990",
             Future_period = "2080s",
             Emissions_scenario = names(fut_clim),
             Link_to_source = "https://adaptwest.databasin.org/pages/adaptwest-climatena-cmip5/"),
  file.path(out_folder, "climate_data_readme.csv"),
  row.names = FALSE
)

# Need to copy the prj file in so that all the crs is used
prj_file <- file.path(dat_pth, "NA_Reference_files_ASCII/ClimateNA_ID.prj")

cross2(c(fut_clim, cur_clim), file_nms) %>%
  map(~file.copy(prj_file, file.path(.x[[1]], paste0(.x[[2]], ".prj"))))

# use first scenario to set breaks
brks_out <- prep_clim_data(mat_norm = file.path(cur_clim, "MAT.asc"),
                          mat_fut = file.path(fut_clim[[1]], "MAT.asc"),
                          cmd_norm = file.path(cur_clim, "CMD.asc"),
                          cmd_fut = file.path(fut_clim[[1]], "CMD.asc"),
                          map = file.path(cur_clim, "MAP.asc"),
                          mwmt = file.path(cur_clim, "MWMT.asc"),
                          mcmt = file.path(cur_clim, "MCMT.asc"),
                          out_folder = out_folder,
                          clim_poly = clim_poly,
                          overwrite = TRUE,
                          scenario_name = names(fut_clim[1]))

purrr::map(names(fut_clim[2:length(fut_clim)]),
           ~prep_clim_data(mat_norm = file.path(cur_clim, "MAT.asc"),
                          mat_fut = file.path(fut_clim[[.x]], "MAT.asc"),
                          cmd_norm = file.path(cur_clim, "CMD.asc"),
                          cmd_fut = file.path(fut_clim[[.x]], "CMD.asc"),
                          map = file.path(cur_clim, "MAP.asc"),
                          mwmt = file.path(cur_clim, "MWMT.asc"),
                          mcmt = file.path(cur_clim, "MCMT.asc"),
                          out_folder = out_folder,
                          clim_poly = clim_poly,
                          overwrite = TRUE,
                          scenario_name = .x,
                          brks_mat = brks_out$brks_mat, brks_cmd = brks_out$brks_cmd,
                          brks_ccei = brks_out$brks_ccei))


beepr::beep()

# make a combined readme to enable choosing from the data package options
rdmes <- list.files(file.path(dat_pth, "ccviR_data_pkg"), pattern = "readme", recursive = TRUE) %>%
  set_names(.)
map_dfr(rdmes, ~read.csv(file.path(dat_pth, "ccviR_data_pkg", .x)),
        .id = "Folder_name") %>%
  mutate(Folder_name = gsub("/climate_data_readme.csv", "", Folder_name)) %>%
  select(-Scenario_Name) %>% group_by(across(-Emissions_scenario)) %>%
  summarise(Emissions_scenarios = paste0(Emissions_scenario, collapse = ", ")) %>%
  write.csv(file.path(dat_pth, "ccviR_data_pkg", "ccviR_Data_Packages",
                      "Data_Package_Descriptions.csv"),
            row.names = FALSE)
