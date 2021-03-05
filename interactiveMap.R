#libraries:
library(ggplot2) #visualization
library(leaflet) #interactive map
library(rgdal) #reading shape-files
library(shiny)
library(data.table) 


#read in NDVI data, add to data table






#do some statistical computing (standard deviance, Persons r, ); add to data table








#create interactiv map:


#leipzig_Bezirke = readOGR(dsn = "./shapefiles/Leipzig_Stadtbezirke_UTM33N", layer = "sbz", use_iconv = TRUE, encoding = "UTF-8") #leipzig_Bezirke is geospatial object
#leipzig_Ortsteile = readOGR(dsn = "./shapefiles/Leipzig_Ortsteile_UTM33N", layer = "ot", use_iconv = TRUE, encoding = "UTF-8") #leipzig_Ortsteile is geospatial object


# first read in shapefiles of ortsteile and bezirke:
lpz_Bezirke <- sf::read_sf(dsn = './shapefiles/Leipzig_Stadtbezirke_UTM33N/sbz.shp', layer = "sbz") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

lpz_Ortsteile <- sf::read_sf(dsn = "./shapefiles/Leipzig_Ortsteile_UTM33N", layer = "ot") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')



# 
# #some tests for fun and information:
# summary(lpz_Bezirke)
# length(leipzig_Bezirke) #number of elemets in data frame(10)
# head(leipzig_Bezirke@data)
# summary(leipzig_Ortsteile)
# length(leipzig_Ortsteile) #number of elemets in data frame(63)
# head(leipzig_Ortsteile@data)


# #plot the shapefiles with ggplot (preferable to plot)   ---> not used in actual program, just for testing purposes
# ggplot() +
#   geom_polygon(data = leipzig_Bezirke, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
#   theme_void()
# 
# ggplot() +
#   geom_polygon(data = leipzig_Ortsteile, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
#   theme_void()


#using leaflet to create interactive map   ---> used in actual program instead of ggplot
#variante1:

leaflet(lpz_Bezirke) %>%
  addTiles() %>% 
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))
#variante2:

map1 <- leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=lpz_Bezirke, highlightOptions = highlightOptions(color = "white", weight = 2,
                                                              bringToFront = TRUE))
map1



#create shiny application:



