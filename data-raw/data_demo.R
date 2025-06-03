## code to prepare `demo_data` dataset and save files to demo app

devtools::load_all(".")
library(dplyr)
library(sf)
library(terra)
library(purrr)
library(fs)

# TODO: Consider adding code to create the assessment area polygon (?)

pth <- path_package("ccviR", "extdata")

# Mini datasets -------------------------------------------------------------

## Protected areas ------------------
# Include Protected areas where overlaps assessment area provided
protected <- st_read("misc/protected_areas/pa_north_america_no_sm.gpkg")

range <- st_read(test_files()[["assess_poly_pth"]]) %>%
  st_buffer(2000) %>%
  st_transform(st_crs(protected))

# Make file smaller
protected <- st_crop(protected, range) %>%
  st_simplify(dTolerance = 1000)

if(FALSE) {
  library(ggplot2)
  ggplot() +
    geom_sf(data = range) +
    geom_sf(data = protected, fill = "green")
}

# Use shp because for such a small file, seems to be a lighter weight option than gpkg
st_write(protected, path(pth, "protected_areas.shp"), append = FALSE)



## Non-breeding range -----------------------
non_breed0 <- rnaturalearth::ne_states("united states of america", returnclass = "sf") %>%
  filter(name == "Florida") %>%
  select(name)

non_breed <- st_simplify(non_breed0, dTolerance = 1000) %>%
  st_transform(crs = "ESRI:102008") # Good for area calculations

#plot(non_breed0[1,])
#plot(non_breed[1,])

st_write(non_breed, path(pth, "non_breed.shp"), append = FALSE)



## CCEI -----------------------------
# Clip CCEI rasters and move to demo data (raw and processed)
non_breed_clip <- st_transform(
  non_breed,
  st_crs(rast(path("misc", "ccei", "ccei_ssp245.tif")))) %>%
  st_buffer(25000)

rast(path("misc", "ccei", "ccei_ssp245.tif")) |>
  crop(non_breed_clip) |>
  writeRaster(path(pth, "clim_files", "raw", "ccei_ssp245_fl.tif"),
              overwrite = TRUE)

rast(path("misc", "ccei", "ccei_ssp585.tif")) |>
  crop(non_breed_clip) |>
  writeRaster(path(pth, "clim_files", "raw", "ccei_ssp585_fl.tif"),
              overwrite = TRUE)

rast(path("misc", "ccei_processed", "CCEI_reclassRCP_4.5.tif")) |>
  crop(non_breed_clip) |>
  writeRaster(path(pth, "clim_files", "processed", "CCEI_reclassRCP_4.5.tif"),
              overwrite = TRUE)

rast(path("misc", "ccei_processed", "CCEI_reclassRCP_8.5.tif")) |>
  crop(non_breed_clip) |>
  writeRaster(path(pth, "clim_files", "processed", "CCEI_reclassRCP_8.5.tif"),
              overwrite = TRUE)


# Climate Data #================================================================

# location of climate data that all other paths will be relative to
base_pth <-  "D:/Ilona/Climate_data/data/"

# plan is to use a subset of the NA climate data ie a province and potentially
# aggregate the raster to save space


# raw climate data
# location of folders with future climate data names will become scenario names
fut_clim <- list(`RCP 4.5` = file.path(base_pth, "NA_ENSEMBLE_rcp45_2050s_Bioclim_ASCII"),
                 `RCP 8.5` = file.path(base_pth, "NA_ENSEMBLE_rcp85_2050s_Bioclim_ASCII"))

# location of folder with climate normals data
cur_clim <- file.path(base_pth, "NA_NORM_6190_Bioclim_ASCII")

file_nms <- c("MAT", "CMD", "MAP", "MWMT", "MCMT")

# Need to copy the prj file in so that all the crs is used
prj_file <- file.path(base_pth, "NA_Reference_files_ASCII/ClimateNA_ID.prj")

cross2(c(fut_clim, cur_clim), file_nms) %>%
  map(~file.copy(prj_file, file.path(.x[[1]], paste0(.x[[2]], ".prj"))))

clim_fls <- cross2(fut_clim, c("MAT", "CMD")) %>%
  set_names(cross2(names(fut_clim), c("MAT", "CMD")) %>%
              map(~paste(unlist(.x), collapse = "_"))) %>%
  c(cross2(cur_clim, file_nms) %>%
      set_names(cross2("norm", file_nms) %>%
                  map(~paste(unlist(.x), collapse = "_")))) %>%
  map(~list.files(.x[[1]], pattern = paste0(.x[[2]], ".asc"), full.names = TRUE))

clim_na <- map(clim_fls, raster::raster)

# get Canadian provinces
can_poly <- raster::getData("GADM", country = "CAN", level = 1)

can_poly <- st_as_sf(can_poly)

assess_poly <- filter(can_poly, NAME_1 == "New Brunswick") %>%
  st_transform(st_crs(clim_na[[1]]))

# crop to NB and aggregate to make files small
clim_nb <- terra::rast(purrr::map(clim_fls, terra::rast)) %>% terra::crop(assess_poly) %>%
  terra::aggregate(fact = 10, fun = mean)

raster::writeRaster(clim_nb, "inst/extdata/clim_files/raw/NB",
                    format = "GTiff", bylayer = TRUE,
                    suffix = "names", overwrite = TRUE)

# Species data #================================================================
# get ecoregions to use as demo range
unzip("../Climate_data/data/ecoregion_shp.zip")

ecoreg<- read_sf("Ecoregions/ecoregions.shp")

rng_poly <- filter(ecoreg, ECOREGION %in% c(118, 119)) %>%
  st_transform(st_crs(clim_na[[1]])) %>%
  st_intersection(assess_poly)

# and use 119 as PTN
ptn_poly <- rng_poly %>% filter(ECOREGION == 119) %>% select(ECOZONE)

rng_poly <- rng_poly %>% summarise(ECOZONE = first(ECOZONE)) %>%
  st_buffer(-100) # avoid validity warning when mapping

# make HS rasts by combining clim_dat mat+cmd

hs_45 <- (clim_nb$RCP.4.5_MAT*100+clim_nb$RCP.4.5_CMD) < 600 &
  (clim_nb$RCP.4.5_MAT*100+clim_nb$RCP.4.5_CMD) > 200

hs_85 <- (clim_nb$RCP.8.5_MAT*100+clim_nb$RCP.8.5_CMD) < 600 &
  (clim_nb$RCP.8.5_MAT*100+clim_nb$RCP.8.5_CMD) > 200

hs_norm <- (clim_nb$norm_MAT*100+clim_nb$norm_CMD) < 600 &
  (clim_nb$norm_MAT*100+clim_nb$norm_CMD) > 200

hs_norm <- raster::mask(hs_norm, rng_poly)

# 0 is maintained 1 is gained, -1 would be lost
rng_chg_45 <- hs_45 - hs_norm

rng_chg_85 <- hs_85 - hs_norm


sp_dat <- lst(rng_poly, rng_chg_45, rng_chg_85, assess_poly, ptn_poly)

write_fun <- function(x, nm, dir){
  if(inherits(x, "Raster")){
    if(nm == "CCEI"){
      raster::writeRaster(x, paste0(dir, nm, ".img"), overwrite = TRUE)
    } else {
      raster::writeRaster(x, paste0(dir, nm, ".tif"), overwrite = TRUE)
    }
  }
  if(inherits(x, "sf")){
    write_sf(sf::st_make_valid(x), paste0(dir, nm, ".shp"))
  }
}

purrr::walk2(sp_dat, names(sp_dat), write_fun,
             dir = "inst/extdata/")

# Prepare the data #============================================================
in_folder <- "inst/extdata/clim_files/raw/"

# use first scenario to set breaks



brks_out <- prep_clim_data(mat_norm = file.path(in_folder, "NB_norm_MAT.tif"),
                           mat_fut = file.path(in_folder, "NB_RCP.4.5_MAT.tif"),
                           cmd_norm = file.path(in_folder, "NB_norm_CMD.tif"),
                           cmd_fut = file.path(in_folder, "NB_RCP.4.5_CMD.tif"),
                           map = file.path(in_folder, "NB_norm_MAP.tif"),
                           mwmt = file.path(in_folder, "NB_norm_MWMT.tif"),
                           mcmt = file.path(in_folder, "NB_norm_MCMT.tif"),
                           out_folder = "inst/extdata/clim_files/processed/",
                           clim_poly = "inst/extdata/assess_poly.shp",
                           overwrite = TRUE,
                           scenario_name = "RCP 4.5")

prep_clim_data(mat_norm = file.path(in_folder, "NB_norm_MAT.tif"),
              mat_fut = file.path(in_folder, "NB_RCP.8.5_MAT.tif"),
              cmd_norm = file.path(in_folder, "NB_norm_CMD.tif"),
              cmd_fut = file.path(in_folder, "NB_RCP.8.5_CMD.tif"),
              map = file.path(in_folder, "NB_norm_MAP.tif"),
              mwmt = file.path(in_folder, "NB_norm_MWMT.tif"),
              mcmt = file.path(in_folder, "NB_norm_MCMT.tif"),
              out_folder = "inst/extdata/clim_files/processed/",
              clim_poly = "inst/extdata/assess_poly.shp",
              overwrite = TRUE,
              scenario_name = "RCP 8.5",
              brks_mat = brks_out$brks_mat,
              brks_cmd = brks_out$brks_cmd)

# make readme csv
write.csv(
  data.frame(Scenario_Name = names(fut_clim),
             GCM_or_Ensemble_name = "AdaptWest 15 CMIP5 AOGCM Ensemble",
             Historical_normal_period = "1961-1990",
             Future_period = "2050s",
             Emissions_scenario = names(fut_clim),
             Link_to_source = "https://adaptwest.databasin.org/pages/adaptwest-climatena-cmip5/",
             brks_mat = brks_out$brks_mat %>% brks_to_txt(),
             brks_cmd = brks_out$brks_cmd %>% brks_to_txt(),
             brks_ccei = brks_out$brks_ccei %>% brks_to_txt()),
  file.path("inst/extdata/clim_files/processed/", "climate_data_readme.csv"),
  row.names = FALSE
)

# Use the data #============================================================
rng_poly <- read_sf("inst/extdata/rng_poly.shp", agr = "constant")
assess_poly <- read_sf("inst/extdata/assess_poly.shp", agr = "constant")
HS_rast_high <- raster::stack(raster("inst/extdata/rng_chg_45.tif"),
                              raster("inst/extdata/rng_chg_85.tif"))
PTN_poly <- read_sf("inst/extdata/PTN_poly.shp", agr = "constant")

spat_res <- analyze_spatial(range_poly = rng_poly, scale_poly = assess_poly,
                        ptn_poly = PTN_poly,
                        hs_rast = HS_rast_high,
                        hs_rcl = matrix(c(c(-1, 0), c(1, 2)), ncol = 2),
                        scenario_names = names(fut_clim),
                        clim_vars_lst = get_clim_vars("inst/extdata/clim_files/processed/",
                                                      scenario_names = names(fut_clim)))

vuln_df <- make_vuln_df("sp_name", 0)

vuln_df$Value1[1:15] <- c(0,0, 0,0,0,0, -1, -1, -1, -1, 0, 0, 0, 0, 0)
vuln_df$Value1[26:29] <- c(0, -1, -1, 0)

res <- calc_vulnerability(spat_res$spat_table, vuln_df, tax_grp = "Bird")

unlink("Ecoregions", recursive = TRUE)
unlink("gadm36_CAN_1_sp.rds", recursive = TRUE)

# this is supposed to fix the executable files error in R-CMD-CHK
# see: https://stackoverflow.com/questions/70713010/convincing-r-that-the-dbf-file-associated-with-a-shp-file-is-not-an-executable
not_exe_fun <- function(fn){
  sz = file.info(fn)$size
  r = readBin(fn, raw(), sz)
  r[2] = as.raw(121) ## make it 2021 instead of 2022
  writeBin(r, fn)
}

list.files("inst/extdata", pattern = "dbf", recursive = TRUE, full.names = TRUE) %>%
  purrr::walk(not_exe_fun)
