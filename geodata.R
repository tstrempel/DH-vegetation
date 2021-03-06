library(sen2r)
library(raster)
library(rgdal)
library(rasterVis)
library(magrittr)
library(dplyr)

sen2r("sen2r_parameters/test3.json")

tif_ndvi <- raster("sen2r_output/NDVI/S2A2A_20200806_022_sen2r_NDVI_10.tif")
tif_evi <- raster("sen2r_output/NDVI/S2A2A_20200806_022_sen2r_NDVI_10.tif")

shp_sbz <- shapefile("data/Leipzig_Stadtbezirke_UTM33N/sbz.shp")
shp_ot <- shapefile("data/Leipzig_Ortsteile_UTM33N/ot.shp")

mean_sbz_ndvi <- raster::extract(tif_ndvi, shp_sbz, fun = mean) %>% data.frame() %>% cbind(shp_sbz$Name)
colnames(mean_sbz_ndvi) <- c("NDVI", "SBZ")

mean_sbz_evi <- raster::extract(tif_evi, shp_sbz, fun = mean) %>% data.frame() %>% cbind(name = shp_sbz$Name)
colnames(mean_sbz_evi) <- c("EVI", "SBZ")

mean_ot_ndvi <- raster::extract(tif_ndvi, shp_ot, fun = mean) %>% data.frame() %>% cbind(name = shp_ot$Name)
colnames(mean_ot_ndvi) <- c("NDVI", "OT")

mean_ot_evi <- raster::extract(tif_evi, shp_ot, fun = mean) %>% data.frame() %>% cbind(name = shp_ot$Name)
colnames(mean_ot_evi) <- c("EVI", "OT")

write.csv(mean_sbz_ndvi, "data/mean_sbz_ndvi.csv", row.names = FALSE)
write.csv(mean_sbz_evi, "data/mean_sbz_evi.csv", row.names = FALSE)
write.csv(mean_ot_ndvi, "data/mean_ot_ndvi.csv", row.names = FALSE)
write.csv(mean_ot_ndvi, "data/mean_ot_evi.csv", row.names = FALSE)

# TODO:
# Wolken in sen2r -> NA
# Wasser -> NA