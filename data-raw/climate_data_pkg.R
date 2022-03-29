## code to prepare `climate_data_pkg` to store climate data on OSF to be
## downloaded by the app

# Download the climate data from adaptwest

fut_clim <- "../Climate_data/data/NA_ENSEMBLE_rcp45_2050s_Bioclim_ASCII/"

cur_clim <- "../Climate_data/data/NA_NORM_8110_Bioclim_ASCII"

# Need to copy the prj file in so that all the crs is used
prj_file <- "../Climate_data/data/NA_Reference_files_ASCII/ClimateNA_ID.prj"

file_nms <- c("MAT", "CMD", "MAP", "MWMT", "MCMT")

lapply(file_nms, function(x){
  x <- file.path(cur_clim, paste0(x, ".prj"))
  file.copy(prj_file, x)
})

run_prep_data(mat_norm = file.path(cur_clim, "MAT.asc"),
              mat_fut = file.path(fut_clim, "MAT.asc"),
              cmd_norm = file.path(cur_clim, "CMD.asc"),
              cmd_fut = file.path(fut_clim, "CMD.asc"),
              map = file.path(cur_clim, "MAP.asc"),
              mwmt = file.path(cur_clim, "MWMT.asc"),
              mcmt = file.path(cur_clim, "MCMT.asc"),
              ccei = "../CCVI_analysis/data/clim_files/raw/ccei.img",
              out_folder = "../Climate_data/data/ccviR_data_pkg/CMIP5_ENSEMBLE_rcp45_2050_NORM_8110",
              clim_poly = file.path("../Climate_data/data/ccviR_data_pkg/CMIP5_ENSEMBLE_rcp45_2050_NORM_8110", "clim_poly.shp"),
              overwrite = TRUE)
beepr::beep()

# make readme csv
write.csv(
  tibble::tribble(~`Scenario Name`,~`GCM or Ensemble name`,~`Historical normal period`,~`Future period`,~`Emissions scenario`,~`Link to source`,
                  "Scenario 1","AdaptWest 15 CMIP5 AOGCM Ensemble","1981-2010","2050s","RCP 4.5","https://adaptwest.databasin.org/pages/adaptwest-climatena-cmip5/"),
  "../Climate_data/data/ccviR_data_pkg/CMIP5_ENSEMBLE_rcp45_2050_NORM_8110/climate_data_readme.csv",
  row.names = FALSE
)

