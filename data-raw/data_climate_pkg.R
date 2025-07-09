## code to prepare `climate_data_pkg` to store climate data on google drive to be

# location of climate data that all other paths will be relative to
base_pth <-  "D:/Ilona/Climate_data/data/"
devtools::load_all(".")

#' Prepare climate data from Adaptwest

make_clim_dat_pkg <- function(base_pth, out_pth, norm_pth, fut_pth, clim_poly_pth,
                              prj_pth, Scenario_Name,
                              GCM_or_Ensemble_name,
                              Historical_normal_period,
                              Future_period ,
                              Emissions_scenario,
                              Link_to_source,
                              do_clip = FALSE){
  out_folder <- file.path(base_pth, out_pth)

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

  # Create a polygon for just Canada
  # sf::read_sf(clim_poly) %>% filter(GID_0 == "CAN") %>%
  #   sf::write_sf(file.path(base_pth, "Canada_w_sable.shp"))

  # location of file with area that climate data is from in this case NA
  clim_poly <- file.path(base_pth, clim_poly_pth)

  # location of folders with future climate data names will become scenario names
  fut_clim <- purrr::map(fut_pth, ~file.path(base_pth, .x))

  # location of folder with climate normals data
  cur_clim <- file.path(base_pth, norm_pth)

  file_nms <- c("MAT", "CMD", "MAP", "MWMT", "MCMT")

  # Need to copy the prj file in so that all the crs is used
  prj_file <- file.path(base_pth, prj_pth)

  purrr::cross2(c(fut_clim, cur_clim), file_nms) %>%
    purrr::map(~file.copy(prj_file, file.path(.x[[1]], paste0(.x[[2]], ".prj"))))

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

  # make readme csv
  rdme <- lst(Scenario_Name, GCM_or_Ensemble_name, Historical_normal_period,
             Future_period, Emissions_scenario, Link_to_source) %>%
    as.data.frame() %>%
    mutate(brks_mat = brks_out$brks_mat %>% brks_to_txt(),
           brks_cmd = brks_out$brks_cmd %>% brks_to_txt(),
           brks_ccei = brks_out$brks_ccei %>% brks_to_txt())

  write.csv(rdme, file.path(out_folder, "climate_data_readme.csv"),
            row.names = FALSE)


  beepr::beep()
}



# make one data package
# make_clim_dat_pkg(
#   base_pth,
#   out_pth = "ccviR_data_pkg/CMIP5_ENSEMBLE_rcp45_rcp85_2080_NORM_6190",
#   norm_pth = "NA_NORM_6190_Bioclim_ASCII",
#   fut_pth = list(`RCP 4.5` = "NA_ENSEMBLE_rcp45_2080s_Bioclim_ASCII",
#                  `RCP 8.5` = "NA_ENSEMBLE_rcp85_2080s_Bioclim_ASCII"),
#   clim_poly_pth = "North_Am_w_sable.shp",
#   prj_pth = "NA_Reference_files_ASCII/ClimateNA_ID.prj",
#   Scenario_Name = c("RCP 4.5", "RCP 8.5"),
#   GCM_or_Ensemble_name = "AdaptWest 15 CMIP5 AOGCM Ensemble",
#   Historical_normal_period = "1961-1990",
#   Future_period = "2080s",
#   Emissions_scenario = c("RCP 4.5", "RCP 8.5"),
#   Link_to_source = "https://adaptwest.databasin.org/pages/adaptwest-climatena-cmip5/",
#   do_clip = FALSE
# )

# use data package descriptions to get list different versions
data_pkg_tbl <- read.csv(file.path(base_pth, "ccviR_data_pkg", "ccviR_Data_Packages",
                                   "Data_Package_Make_Table.csv")) %>%
  rowwise() %>%
  mutate(Emissions_scenario = list(Emissions_scenario %>% stringr::str_split_1(", ")),
         Scenario_Name = list(Emissions_scenario),
         fut_pth = stringr::str_split(fut_pth, ";") %>% as.list(),
         base_pth = base_pth,
         .keep = "unused")

purrr::pmap(data_pkg_tbl, make_clim_dat_pkg)

# make a combined readme to enable choosing from the data package options
rdmes <- list.files(file.path(base_pth, "ccviR_data_pkg"), pattern = "readme",
                    recursive = TRUE) %>%
  purrr::set_names(.)

purrr::map_dfr(rdmes, ~read.csv(file.path(base_pth, "ccviR_data_pkg", .x)),
        .id = "Folder_name") %>%
  mutate(Folder_name = gsub("/climate_data_readme.csv", "", Folder_name)) %>%
  select(-Scenario_Name, -contains("brks")) %>% group_by(across(-Emissions_scenario)) %>%
  summarise(Emissions_scenarios = paste0(Emissions_scenario, collapse = ", ")) %>%
  write.csv(file.path(base_pth, "ccviR_data_pkg", "ccviR_Data_Packages",
                      "Data_Package_Descriptions.csv"),
            row.names = FALSE)
