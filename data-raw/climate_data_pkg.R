## code to prepare `climate_data_pkg` to store climate data on OSF to be

library(purrr)
# Download the climate data from adaptwest
out_folder <- "../Climate_data/data/ccviR_data_pkg/CMIP5_ENSEMBLE_rcp45_rcp85_2050_NORM_6190"

if(!dir.exists(out_folder)){
  dir.create(out_folder)
}

# location of file with area that climate data is from in this case NA
clim_poly <- file.path("../Climate_data/data/ccviR_data_pkg/CMIP5_ENSEMBLE_rcp45_2050_NORM_8110", "clim_poly.shp")

# location of folders with future climate data names will become scenario names
fut_clim <- list(`RCP 4.5` = "../Climate_data/data/NA_ENSEMBLE_rcp45_2050s_Bioclim_ASCII",
                 `RCP 8.5` = "../Climate_data/data/NA_ENSEMBLE_rcp85_2050s_Bioclim_ASCII")

# location of folder with climate normals data
cur_clim <- "../Climate_data/data/NA_NORM_6190_Bioclim_ASCII"

file_nms <- c("MAT", "CMD", "MAP", "MWMT", "MCMT")

# make readme csv
write.csv(
  data.frame(`Scenario Name` = names(fut_clim),
             `GCM or Ensemble name` = "AdaptWest 15 CMIP5 AOGCM Ensemble",
             `Historical normal period` = "1961-1990",
             `Future period` = "2050s",
             `Emissions scenario` = names(fut_clim),
             `Link to source` = "https://adaptwest.databasin.org/pages/adaptwest-climatena-cmip5/"),
  file.path(out_folder, "climate_data_readme.csv"),
  row.names = FALSE
)

# Need to copy the prj file in so that all the crs is used
prj_file <- "../Climate_data/data/NA_Reference_files_ASCII/ClimateNA_ID.prj"

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



