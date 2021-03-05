library(sen2r)
library(raster)
library(rgdal)
library(rasterVis)

sen2r("sen2r_parameters/test3.json")

shp <- shapefile("shapefiles/Leipzig_Stadtbezirke_UTM33N/sbz.shp")

GDALinfo("sen2r_output/NDVI/S2B2A_20200806_022_sen2r_NDVI_10.tif")
tif <- raster("sen2r_output/NDVI/S2B2A_20200806_022_sen2r_NDVI_10.tif")
plot(tif)

# TODO:
# Wolken in sen2r -> NA
# Wasser -> NA
# Bereich aus dem tif für einen Stadtteil selektieren und Durschnittlichen NDVI darüber berechnen

mean_values <- extract(tif, shp, fun = mean)
output <- data.frame(mean_values)
output <- cbind(output, shp$Name)
print(output)

# masked <- mask(tif, shp)
# plot(masked)