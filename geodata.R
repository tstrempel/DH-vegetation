library(sen2r)
library(raster)
library(rgdal)

sen2r("sen2r_parameters/test2.json")

shp <- shapefile("shapefiles/Leipzig_Stadtbezirke_UTM33N/sbz.shp")
GDALinfo("sen2r_output/NDVI/S2B2A_20200801_022_sen2r_NDVI_10.tif")

# TODO:
# Wolken in sen2r -> NA
# Wasser -> NA
# Bereich aus dem tif für einen Stadtteil selektieren und Durschnittlichen NDVI darüber berechnen