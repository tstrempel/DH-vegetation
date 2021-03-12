library(raster)
library(rgdal)

# use this for NDVI:
# compute_vegetation_index("data/NDVI.tif", circle)
# or for EVI:
# compute_vegetation_index("data/EVI.tif", circle)
compute_vegetation_index <- function(file, shp) {
  tif <- raster(file)
  tif[tif < 0] <- NA
  
  mean <- raster::extract(tif_ndvi, shp, na.rm = TRUE, fun = mean)
}