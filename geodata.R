library(sen2r)
library(raster)
library(rgdal)
library(magrittr)
library(dplyr)

source("vegetationIndex.R")

sen2r("sen2r_parameters/test3.json")

tif_ndvi <- raster("data/NDVI.tif")
tif_evi <- raster("data/EVI.tif")

shp_sbz <- shapefile("data/Leipzig_Stadtbezirke_UTM33N/sbz.shp")
shp_ot <- shapefile("data/Leipzig_Ortsteile_UTM33N/ot.shp")

mean_sbz_ndvi <- compute_vegetation_index(tif_ndvi, shp_sbz)  %>% data.frame() %>% cbind(shp_sbz$Name)
colnames(mean_sbz_ndvi) <- c("NDVI", "Name")

mean_sbz_evi <- compute_vegetation_index(tif_evi, shp_sbz) %>% data.frame() %>% cbind(name = shp_sbz$Name)
colnames(mean_sbz_evi) <- c("EVI", "Name")

mean_ot_ndvi <- compute_vegetation_index(tif_ndvi, shp_ot) %>% data.frame() %>% cbind(name = shp_ot$Name)
colnames(mean_ot_ndvi) <- c("NDVI", "Name")

mean_ot_evi <- compute_vegetation_index(tif_evi, shp_ot) %>% data.frame() %>% cbind(name = shp_ot$Name)
colnames(mean_ot_evi) <- c("EVI", "Name")

write.csv(mean_sbz_ndvi, "data/mean_sbz_ndvi.csv", row.names = TRUE)
write.csv(mean_sbz_evi, "data/mean_sbz_evi.csv", row.names = TRUE)
write.csv(mean_ot_ndvi, "data/mean_ot_ndvi.csv", row.names = TRUE)
write.csv(mean_ot_evi, "data/mean_ot_evi.csv", row.names = TRUE)

# TODO:
# Wolken in sen2r -> NA