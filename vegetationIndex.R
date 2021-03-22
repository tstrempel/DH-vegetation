library(raster)
library(rgdal)

# use this for NDVI:
# compute_vegetation_index(ndvi_tif, circle)
# or for EVI:
# compute_vegetation_index(evi_tif, circle)
compute_vegetation_index <- function(raster_layer, spatial_data) {
  # due to measurement errors dense city landscapes can have low negative values, set these to 0
  raster_layer[raster_layer >= -0.1 & raster_layer <= 0] <- 0
  # water has high negative values, set them to NA so that they are not used in later calculations
  raster_layer[raster_layer < 0] <- NA
  # NDVI/EVI can't be higher than 1
  raster_layer[raster_layer > 1] <- NA
  
  mean <- raster::extract(raster_layer, spatial_data, na.rm = TRUE, fun = mean)
}