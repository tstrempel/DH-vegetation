library(sen2r)
library(raster)
library(rgdal)
library(magrittr)
library(dplyr)
library(snow)

source("vegetationIndex.R")

# create dirs
dir.create("sen2r_data")
dir.create("sen2r_output")
# download all needed data and compute NDVI/EVI(this will take a while)
sen2r("sen2r_parameters/final.json")

# turn .tif files into raster objects
# only select pictures with low cloud cover
tif_ndvi_files = c("sen2r_output/NDVI/S2A2A_20200730_065_sen2r_NDVI_10.tif", "sen2r_output/NDVI/S2A2A_20200816_022_sen2r_NDVI_10.tif", "sen2r_output/NDVI/S2A2A_20200908_065_sen2r_NDVI_10.tif", "sen2r_output/NDVI/S2A2A_20200915_022_sen2r_NDVI_10.tif", "sen2r_output/NDVI/S2A2A_20200918_065_sen2r_NDVI_10.tif", "sen2r_output/NDVI/S2A2A_20200928_065_sen2r_NDVI_10.tif", "sen2r_output/NDVI/S2B2A_20200602_022_sen2r_NDVI_10.tif", "sen2r_output/NDVI/S2B2A_20200801_022_sen2r_NDVI_10.tif", "sen2r_output/NDVI/S2B2A_20200811_022_sen2r_NDVI_10.tif", "sen2r_output/NDVI/S2B2A_20200821_022_sen2r_NDVI_10.tif", "sen2r_output/NDVI/S2B2A_20200913_065_sen2r_NDVI_10.tif")
tif_evi_files = c("sen2r_output/EVI/S2A2A_20200730_065_sen2r_EVI_10.tif", "sen2r_output/EVI/S2A2A_20200816_022_sen2r_EVI_10.tif", "sen2r_output/EVI/S2A2A_20200908_065_sen2r_EVI_10.tif", "sen2r_output/EVI/S2A2A_20200915_022_sen2r_EVI_10.tif", "sen2r_output/EVI/S2A2A_20200918_065_sen2r_EVI_10.tif", "sen2r_output/EVI/S2A2A_20200928_065_sen2r_EVI_10.tif", "sen2r_output/EVI/S2B2A_20200602_022_sen2r_EVI_10.tif", "sen2r_output/EVI/S2B2A_20200801_022_sen2r_EVI_10.tif", "sen2r_output/EVI/S2B2A_20200811_022_sen2r_EVI_10.tif", "sen2r_output/EVI/S2B2A_20200821_022_sen2r_EVI_10.tif", "sen2r_output/EVI/S2B2A_20200913_065_sen2r_EVI_10.tif")

tif_ndvi_files <- lapply(tif_ndvi_files, raster)
tif_evi_files <- lapply(tif_evi_files, raster)

# read in the city districts 
shp_sbz <- shapefile("data/Leipzig_Stadtbezirke_UTM33N/sbz.shp")
shp_ot <- shapefile("data/Leipzig_Ortsteile_UTM33N/ot.shp")
shp_sbz$Name = c("Mitte", "Nordost", "Ost", "Südost", "Süd", "Südwest", "West", "Alt-West", "Nordwest", "Nord")
shp_ot$Name = c("Zentrum", "Zentrum-Ost", "Zentrum-Südost", "Zentrum-Süd", "Zentrum-West", "Zentrum-Nordwest", "Zentrum-Nord", "Schönefeld-Abtnaundorf", "Schönefeld-Ost", "Mockau-Süd", "Mockau-Nord", "Thekla", "Plaußig-Portitz", "Neustadt-Neuschönefeld", "Volkmarsdorf", "Anger-Crottendorf", "Sellerhausen-Stünz", "Paunsdorf", "Heiterblick", "Mölkau", "Engelsdorf", "Baalsdorf", "Althen-Kleinpösna", "Reudnitz-Thonberg", "Stötteritz", "Probstheida", "Meusdorf", "Liebertwolkwitz", "Holzhausen", "Südvorstadt", "Connewitz", "Marienbrunn", "Lößnig", "Dölitz-Dösen", "Schleußig", "Plagwitz", "Kleinzschocher", "Großzschocher", "Knautkleeberg-Knauthain", "Hartmannsdorf-Knautnaundorf", "Schönau", "Grünau-Ost", "Grünau-Mitte", "Grünau-Siedlung", "Lausen-Grünau", "Grünau-Nord", "Miltitz", "Lindenau", "Altlindenau", "Neulindenau", "Leutzsch", "Böhlitz-Ehrenberg", "Burghausen-Rückmarsdorf", "Möckern", "Wahren", "Lützschena-Stahmeln", "Lindenthal", "Gohlis-Süd", "Gohlis-Mitte", "Gohlis-Nord", "Eutritzsch", "Seehausen", "Wiederitzsch")

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
