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


# faster project raster
ref_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
rast_pth <- "data/species_files/AWPE/marshlands_AWPE_amwpel_breeding_2055_45_ENSEMBLE_classifiedchange.tif"

out <- wrap_gdalwarp(rast_pth, ref_crs$input, "data/marshlands_AWPE_WGS84.tif",
                     overwrite = TRUE, output_Raster = TRUE)

folder_loc <- "data/hs_files"

fls <- list.files(folder_loc, full.names = TRUE)

map(fls, ~wrap_gdalwarp(.x, ref_crs, str_replace(.x, "change", "changeWGS84")))

hs1 <- raster(fls[[1]])
hs2 <- raster(fls[[2]])
fls[[3]] <- fls[[1]]

ras_stk <- raster::stack(fls)

sum_rasts <- raster::calc(ras_stk, function(x){ sum(x == 1, na.rm = TRUE)},
                          filename = "data/hs_files2/summed_loss.tif")

# # compare results
# input <- raster(rast_pth)
#
# plot(input)
# plot(out)

# seems to be something wrong with CMD file that means it won't plot with leaflet
cmd <- raster("data/clim_files/CMD_delta_reclass_NA_WGS84.tif")
mat <- raster("data/clim_files/MAT_delta_100_reclass_NA.tif")

qtm(cmd) %>% tmap_leaflet()

# Exposure Spatial processing #===============================================
# Run CCVI exposure for one species
AMCO_NA <- run_CCVI_calcs(
  species_nm = "AMCO",
  scale_nm = "NA",
  root_pth = "data"
)

# Run for multiple scales
AMCO_all <- map_df(list("NA", "CAN", "USA"), ~run_CCVI_calcs("AMCO", .x, "data"))

write.csv(AMCO_all, "data/outputs/AMCO_NA_CAN_USA.csv", row.names = FALSE)

# Run for multiple species and scales
results_all <- map_df(list("AMCO", "AWPE"),
                      ~map_df(list("NA", "CAN", "USA"),
                              ~run_CCVI_calcs(.y, .x, "data"), .y = .x))

write.csv(results_all, "data/outputs/AMCO_AWPE_NA_CAN_USA.csv", row.names = FALSE)

# Vulnerability index calculations #===========================================

# convert from results table in excel to input for function to compare
res_tbl <- readxl::read_excel("documents/ccvi_release_3.02_1_jun_2016_0.xlsm",
                   sheet = "Results Table", range = "A5:CI9")

res_tbl2 <- res_tbl %>%
  select_if(~!all(is.na(.x))) %>%
  select(`Taxonomic Group`:Migratory, B1...25:D4...77, -`Geographic Area`) %>%
  rename_all(~stringr::str_remove(.x, "\\.\\.\\.\\d\\d")) %>%
  rename(Z2 = `Cave/GW`, Z3 = Migratory) %>%
  mutate(Z2 = ifelse(is.na(Z2), 0, Z2),
         Z3 = ifelse(is.na(Z2), 0, Z3)) %>%
  tidyr::pivot_longer(c(-Species, -`Taxonomic Group`),
                      names_to = "Code",
                      values_to = "Value") %>%
  tidyr::separate(Value, c("Value1", "Value2", "Value3", "Value4"), fill = "right",
                  sep = "-") %>%
  mutate_at(vars(contains("Value")), ~case_when(.x == "GI" ~ 3,
                                                .x == "Inc" ~ 2,
                                                .x == "SI" ~ 1,
                                                .x == "N" ~ 0,
                                                .x == "U" ~ -1,
                                                .x == "N/A" ~ NA_real_,
                                                .x == "" ~ NA_real_,
                                                .x == "X" ~ 1,
                                                .x == "x" ~ 1,
                                                .x == 0 ~ 0,
                                                is.na(.x) ~ NA_real_,
                                                TRUE ~ 999)) %>%
  distinct() %>%
  filter(`Taxonomic Group` != "Amphibian",
         !Code %in% c(c("C2ai", "C2aii", "C2bi", "D2", "D3")))

index_from_excel <- res_tbl %>% select(Species, Index:Confidence)

res_tbl_lst <- split(res_tbl2, f = res_tbl2$Species)

exp_dfs <- read.csv("data/outputs/AMCO_NA_CAN_USA.csv") %>% split(.$scale)

index_calc <-  purrr::map2(rev(res_tbl_lst), exp_dfs,
                           ~calc_vulnerability(exp_df = .y, vuln_df = .x))

data.frame(Species = names(index_calc),
           index = c(index_calc[[1]]$index, index_calc[[2]]$index),
           mig_exp = c(index_calc[[1]]$mig_exp, index_calc[[2]]$mig_exp),
           conf = c(index_calc[[1]]$conf_index, index_calc[[2]]$conf_index))
