library(sen2r)
library(raster)
library(rgdal)
library(magrittr)
library(dplyr)
library(snow)

source("vegetationIndex.R")

sen2r("sen2r_parameters/final.json")

# turn .tif files into raster objects
tif_ndvi_files = list.files(path="sen2r_mean/NDVI/", pattern="*.tif", full.names=TRUE, recursive=FALSE)
tif_evi_files = list.files(path="sen2r_mean/EVI/", pattern="*.tif", full.names=TRUE, recursive=FALSE)
tif_ndvi_files <- lapply(tif_ndvi_files, raster)
tif_evi_files <- lapply(tif_evi_files, raster)

# read in the city districts 
shp_sbz <- shapefile("data/Leipzig_Stadtbezirke_UTM33N/sbz.shp")
shp_ot <- shapefile("data/Leipzig_Ortsteile_UTM33N/ot.shp")

# use 6 cores in parallel cluster to compute means faster
cl <- makeCluster(6, type = "SOCK")

# export all functions into cluster
ex <- Filter(function(x) is.function(get(x, .GlobalEnv)), ls(.GlobalEnv))
clusterExport(cl, ex)
clusterEvalQ(cl, {library(raster)}) 

# export raster objects into the clkuster
clusterExport(cl, "tif_ndvi_files")
clusterExport(cl, "tif_evi_files")

# compute NDVI and EVI in parallel 
sbz_ndvi <- parSapply(cl, tif_ndvi_files, compute_vegetation_index, spatial_data=shp_sbz)
sbz_evi <- parSapply(cl, tif_evi_files, compute_vegetation_index, spatial_data=shp_sbz)
ot_ndvi <- parSapply(cl, tif_ndvi_files, compute_vegetation_index, spatial_data=shp_ot)
ot_evi <- parSapply(cl, tif_evi_files, compute_vegetation_index, spatial_data=shp_ot)

# turn results into data frame and aggregate over the rows with mean
mean_sbz_ndvi <- data.frame(ID=sbz_ndvi[,0], NDVI=rowMeans(sbz_ndvi[,-1]), Name=shp_sbz$Name)
mean_sbz_evi <- data.frame(ID=sbz_evi[,0], EVI=rowMeans(sbz_evi[,-1]), Name=shp_sbz$Name)
mean_ot_ndvi <- data.frame(ID=ot_ndvi[,0], NDVI=rowMeans(ot_ndvi[,-1]), Name=shp_ot$Name)
mean_ot_evi <- data.frame(ID=ot_evi[,0], EVI=rowMeans(ot_evi[,-1]), Name=shp_ot$Name)

# write results into CSVs
write.csv(mean_sbz_ndvi, "data/sbz_ndvi.csv", row.names = TRUE)
write.csv(mean_sbz_evi, "data/sbz_evi.csv", row.names = TRUE)
write.csv(mean_ot_ndvi, "data/ot_ndvi.csv", row.names = TRUE)
write.csv(mean_ot_evi, "data/ot_evi.csv", row.names = TRUE)
