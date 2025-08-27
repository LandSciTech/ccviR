## code to prepare `climate_data_pkg` to store climate data on google drive to be

devtools::load_all(".")

#' Prepare climate data from Adaptwest

make_clim_dat_pkg <- function(base_pth, out_pth, norm_pth, fut_pth, ccei_pth, clim_poly_pth,
                              prj_pth, Scenario_Name,
                              GCM_or_Ensemble_name,
                              Historical_normal_period,
                              Future_period ,
                              Emissions_scenario,
                              Link_to_source,
                              do_clip = FALSE,
                              rast_ext = ".tif"){
  out_folder <- file.path(base_pth, out_pth)

  if(!dir.exists(out_folder)){
    fs::dir_create(out_folder)
  }

  # location of file with area that climate data is from in this case NA
  clim_poly <- file.path(base_pth, clim_poly_pth)

  # location of folders with future climate data names will become scenario names
  fut_clim <- purrr::map(fut_pth, ~file.path(base_pth, .x))

  # location of folder with climate normals data
  cur_clim <- file.path(base_pth, norm_pth)

  # location of folder with CCEI raw data
  ccei_clim <- file.path(base_pth, ccei_pth)

  file_nms <- c("MAT", "CMD", "MAP", "MWMT", "MCMT")

  if(!is.null(prj_pth)){
    # Need to copy the prj file in so that all the crs is used
    prj_file <- file.path(base_pth, prj_pth)

    purrr::cross2(c(fut_clim, cur_clim), file_nms) %>%
      purrr::map(~file.copy(prj_file, file.path(.x[[1]], paste0(.x[[2]], ".prj"))))
  }

  # use first scenario to set breaks
  brks <- prep_clim_data_multi(mat_norm = fs::dir_ls(cur_clim, regexp = "MAT"),
                               mat_fut = purrr::map_chr(fut_clim, \(x)fs::dir_ls(x, regexp = "MAT")),
                               cmd_norm = fs::dir_ls(cur_clim, regexp = "CMD"),
                               cmd_fut = purrr::map_chr(fut_clim, \(x)fs::dir_ls(x, regexp = "CMD")),
                               map = fs::dir_ls(cur_clim, regexp = "MAP"),
                               mwmt = fs::dir_ls(cur_clim, regexp = "MWMT"),
                               mcmt = fs::dir_ls(cur_clim, regexp = "MCMT"),
                               ccei = fs::dir_ls(ccei_clim, regexp = "ccei_ssp"),
                               out_folder = out_folder,
                               clim_poly = clim_poly,
                               overwrite = TRUE,
                               scenario_name = Scenario_Name)



  # make readme csv
  prep_clim_readme(
    scenario_name = Scenario_Name,
    gcm_ensemble = GCM_or_Ensemble_name,
    hist_period = Historical_normal_period,
    fut_period = Future_period,
    emissions_scenario = Emissions_scenario,
    url = Link_to_source,
    out_folder = out_folder,
    brks = brks)


  beepr::beep()
}


# Download climate data from Adaptwest ------------------------------------

dat_url <- "https://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6v73/normals/Normal_1961_1990_bioclim.zip"
dest_dir <- "misc/AdaptWest_climate_raw/CMIP6"
dest_fl <- file.path(dest_dir, "Normal_1961_1990_bioclim.zip")
if(!fs::dir_exists(dest_dir)){
  fs::dir_create(dest_dir)
}
options(timeout = max(4000, getOption("timeout")))
download.file(dat_url, dest_fl, method = "libcurl")


dat_urls <- c(norm = "https://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6v73/normals/Normal_1961_1990_bioclim.zip",
              ssp2 = "https://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6v73/ensembles/ensemble_8GCMs_ssp245_2041_2070_bioclim.zip",
              ssp5 = "https://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6v73/ensembles/ensemble_8GCMs_ssp585_2041_2070_bioclim.zip")

dest_fls <- c(norm = file.path(dest_dir, "Normal_1961_1990_bioclim.zip"),
              ssp2 = file.path(dest_dir, "ensemble_8GCMs_ssp245_2041_2070_bioclim.zip"),
              ssp5 = file.path(dest_dir, "ensemble_8GCMs_ssp585_2041_2070_bioclim.zip"))

download.file(dat_urls, dest_fls, method = "libcurl")

purrr::walk(dest_fls, \(x) unzip(x, exdir = dest_dir))



# Get North America polygon -----------------------------------------------

# GADM is too detailed but does have sable island
clim_poly <- sf::st_as_sf(geodata::gadm(country = c("CAN", "USA", "MEX"), level = 0, resolution = 2, path = "misc"))

# world does not have sable island
NAm <- geodata::world(path = ".") %>% st_as_sf() %>% filter(GID_0 %in% c("CAN", "USA", "MEX"))

# get sable island
plot(clim_poly %>% st_geometry())
ext <- terra::draw()

sable <- st_crop(clim_poly, ext)

NAm2 <- bind_rows(NAm, sable)

write_sf(NAm2, "misc/North_Am_w_sable.shp")

# Create a polygon for just Canada
# sf::read_sf(clim_poly) %>% filter(GID_0 == "CAN") %>%
#   sf::write_sf(file.path(base_pth, "Canada_w_sable.shp"))


# make one data package
make_clim_dat_pkg(
  base_pth = "misc",
  out_pth = "ccviR_data_pkg/CMIP6_ensemble_8GCMs_ssp245_ssp585_2050_NORM_6190",
  norm_pth = "AdaptWest_climate_raw/CMIP6/Normal_1961_1990/Normal_1961_1990_bioclim",
  fut_pth = list(`SSP2-4.5` = "AdaptWest_climate_raw/CMIP6/ensemble_8GCMs_ssp245_2041_2070/ensemble_8GCMs_ssp245_2041_2070_bioclim",
                 `SSP5-8.5` = "AdaptWest_climate_raw/CMIP6/ensemble_8GCMs_ssp585_2041_2070/ensemble_8GCMs_ssp585_2041_2070_bioclim"),
  ccei_pth = "ccei",
  clim_poly_pth = "North_Am_w_sable.shp",
  prj_pth = NULL,
  Scenario_Name = c("SSP2-4.5", "SSP5-8.5"),
  GCM_or_Ensemble_name = "AdaptWest 8 CMIP6 AOGCM Ensemble",
  Historical_normal_period = "1961-1990",
  Future_period = "2050s",
  Emissions_scenario = c("SSP2-4.5", "SSP5-8.5"),
  Link_to_source = "https://adaptwest.databasin.org/pages/adaptwest-climatena/",
  do_clip = FALSE
)

# Zip climate data folder and protected areas
setwd(here::here("misc/ccviR_data_pkg"))
zip("ccviR_data_pkg1.zip",
    files = c("CMIP6_ensemble_8GCMs_ssp245_ssp585_2050_NORM_6190"))

setwd(here::here("misc/protected_areas"))
zip("../ccviR_data_pkg/ccviR_data_pkg1.zip",
    files = c("pa_north_america_no_sm.gpkg"), flag = "-g")
setwd(here::here())

# # use a table to process several sets of climate data
# data_pkg_tbl <- read.csv(file.path(base_pth, "ccviR_data_pkg", "ccviR_Data_Packages",
#                                    "Data_Package_Make_Table.csv")) %>%
#   rowwise() %>%
#   mutate(Emissions_scenario = list(Emissions_scenario %>% stringr::str_split_1(", ")),
#          Scenario_Name = list(Emissions_scenario),
#          fut_pth = stringr::str_split(fut_pth, ";") %>% as.list(),
#          base_pth = base_pth,
#          .keep = "unused")
#
# purrr::pmap(data_pkg_tbl, make_clim_dat_pkg)
#
# # make a combined readme to enable choosing from the data package options
# rdmes <- list.files(file.path(base_pth, "ccviR_data_pkg"), pattern = "readme",
#                     recursive = TRUE) %>%
#   purrr::set_names(.)
#
# purrr::map_dfr(rdmes, ~read.csv(file.path(base_pth, "ccviR_data_pkg", .x)),
#         .id = "Folder_name") %>%
#   mutate(Folder_name = gsub("/climate_data_readme.csv", "", Folder_name)) %>%
#   select(-Scenario_Name, -contains("brks")) %>% group_by(across(-Emissions_scenario)) %>%
#   summarise(Emissions_scenarios = paste0(Emissions_scenario, collapse = ", ")) %>%
#   write.csv(file.path(base_pth, "ccviR_data_pkg", "ccviR_Data_Packages",
#                       "Data_Package_Descriptions.csv"),
#             row.names = FALSE)
