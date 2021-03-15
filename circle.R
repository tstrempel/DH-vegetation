library(raster)
library(rgdal)
library(magrittr)
library(dplyr)

library(spatstat)
library(swfscMisc)
library(sampSurf)

source("vegetationIndex.R")

tif_ndvi <- raster("data/NDVI.tif")
long <- 12.4234771728516
lat <- 51.3494852344741

# with circle.polygon (does not work)
perimeter <- circle.polygon(long, lat, 1, units = "km") %>% data.frame()
coordinates(perimeter) <- c("x", "y")
proj4string(perimeter) <- CRS("+proj=longlat +datum=WGS84")
perimeter <- spTransform(perimeter, CRS("+proj=utm +zone=32 +datum=WGS84 +ellps=WGS84"))
perimeter_ndvi <- compute_vegetation_index(tif_ndvi, perimeter)
perimeter_ndvi

# with spCircle (does work)
point <- data.frame(ID=1, X = c(long), Y = c(lat))
coordinates(point) <- c("X", "Y")
proj4string(point) <- CRS("+proj=longlat +datum=WGS84")
point <- spTransform(point, CRS("+proj=utm +zone=32 +datum=WGS84 +ellps=WGS84")) %>% data.frame()
circle <- spCircle(1000, CRS("+proj=utm +zone=32 +datum=WGS84 +ellps=WGS84"), centerPoint = c(x=point[1,2], y=point[1,3]))
circle_ndvi <- compute_vegetation_index(tif_ndvi, circle$spCircle)