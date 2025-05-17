# Protected Areas Raster for Canada and the USA

This raster is created by `data-raw/data_protected.R` to combine protected areas 
from both countries into one raster for use by the ccviR Shiny app.

Resolution: 1.11 km2
Protected Areas: IUCN Categories I - VI
CRS: 3857

## Sources

### Canada
- Polygons: https://open.canada.ca/data/en/dataset/6c343726-1e92-451a-876a-76e17d398a1c
- Data manual: https://data-donnees.az.ec.gc.ca/api/file?path=%2Fspecies%2Fprotectrestore%2Fcanadian-protected-conserved-areas-database%2FUserManuals%2FProtectedConservedArea_UserManual_2023.pdf

### USA
- Overview: https://www.usgs.gov/programs/gap-analysis-project/science/pad-us-data-download
- Polygons & details: https://www.sciencebase.gov/catalog/item/652ef930d34edd15305a9b03


## Brief overview
Protected area polygons are downloaded, filtered to IUCN Categories I - VI and
to omit Marine areas, and then rasterized using a final projection of CRS 3857
(used by the ccviR Shiny App) The two maps are then combined.
