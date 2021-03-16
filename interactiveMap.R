setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rgdal_show_exportToProj4_warnings = "none"
#update.packages(checkBuilt = TRUE)

#libraries:
library(leaflet) #interactive map
library(leaflet.extras) #additional options for interactive map

library(rgdal) #for spatial data
library(shiny) #web application for hosting
library(data.table) 
library(htmltools)
library(RColorBrewer) #for color palette (shapes)
library(dplyr)
library(spatstat)
library(spatstat.utils)
library(sampSurf)
library(magrittr)


source("VegetationIndex.R")
tif_ndvi <- raster("data/NDVI.tif")


# first read in shapefiles of ortsteile and bezirke; then read in NDVI, EVI and demographical data, add to data set:

#bezirke:
leipzig_Bezirke <- readOGR(dsn = "./shapefiles/Leipzig_Stadtbezirke_UTM33N", layer = "sbz", use_iconv = TRUE, encoding = "UTF-8") #leipzig_Bezirke is geospatial object
leipzig_Bezirke <- spTransform(leipzig_Bezirke, CRS("+proj=longlat +datum=WGS84"))

Bezirke_NDVI <- read.csv(file = './data/mean_sbz_ndvi.csv', row.names=NULL, encoding = "UTF-8")
Bezirke_EVI <- read.csv(file = './data/mean_sbz_evi.csv', row.names=NULL, encoding = "UTF-8")
Bezirke_NDVI <- merge(leipzig_Bezirke, Bezirke_NDVI )
Bezirksdaten_gesamt <- merge(Bezirke_NDVI, Bezirke_EVI) #complete data set Bezirksebene


#ortsteile:
leipzig_Ortsteile = readOGR(dsn = "./shapefiles/Leipzig_Ortsteile_UTM33N", layer = "ot", use_iconv = TRUE, encoding = "UTF-8") #leipzig_Ortsteile is geospatial object
leipzig_Ortsteile <- spTransform(leipzig_Ortsteile, CRS("+proj=longlat +datum=WGS84"))

OT_NDVI <- read.csv(file = './data/mean_ot_ndvi.csv', row.names=NULL, encoding = "UTF-8")
OT_EVI <- read.csv(file = './data/mean_ot_evi.csv', row.names=NULL, encoding = "UTF-8")
OT_NDVI <- merge(leipzig_Ortsteile, OT_NDVI )
Ortsteildaten_gesamt <- merge(OT_NDVI, OT_EVI) #complete data set Ortsteilebene

#wohnumkreis = spCircle(radius= 20, centerPoint=c(x=0,y=0), spID='wohnumkreis')
#plot(wohnumkreis)


#using leaflet and shiny to create interactive map

#first define some labels and colors using html:

labels <- paste("<p>", "Bezirk: ", Bezirksdaten_gesamt$Name,  "</p>",
                "<p>", "NDVI: ", round(Bezirksdaten_gesamt$NDVI, digits = 3) , "</p>",
                "<p>", "EVI: ", round(Bezirksdaten_gesamt$EVI, digits = 3) , "</p>",
                sep= "")

binpal <- colorBin("Greens", Ortsteildaten_gesamt$NDVI, n = 7)

labels2 <- paste("<p>", "Ortsteil: ", Ortsteildaten_gesamt$Name,  "</p>",
                "<p>", "NDVI: ", round(Ortsteildaten_gesamt$NDVI, digits = 3) , "</p>",
                "<p>", "EVI: ", round(Ortsteildaten_gesamt$EVI, digits = 3) , "</p>",
                sep= "")




#compute_vegetation_index() #das muss noch in einen action-button --> fehlerprüfung: kreis vorhanden? dann compute ndvi, einfärben, labeln


#and now the shiny application:

ui <- bootstrapPage(
  #absolutePanel(top = 10, right = 10, fixes = TRUE)
  title = "Green spaces Leipzig",
  titlePanel("Green spaces Leipzig visualization"),
  leafletOutput("map1", height = 900),
  actionButton("Ortsteile_umschalten", "Ortsteile zeigen"),
  checkboxInput("legende", "Zeige Farbskala", TRUE),
  checkboxInput("umkreisBox", "NDVI für Wohnumkreis berechnen", FALSE)

)

server <- function(input, output, session){
  output$map1 <- renderLeaflet({
    
    
    leaflet(options = leafletOptions(minZoom = 9, maxZoom = 15), Bezirksdaten_gesamt) %>%
    addTiles() %>% 
    setView(lat=51.34, lng=12.36, zoom=11) %>% 
    addPolygons(
        data = Bezirksdaten_gesamt,
        group = "Bezirke",
        weight = 2, 
        smoothFactor = 0.5,
        opacity = 0.5, 
        fillOpacity = 0.8, 
        color = ~binpal( Bezirksdaten_gesamt$NDVI),
        highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
        label =  lapply(labels, HTML),
        layerId = ~Name #to identify which shape is clicked
        
      ) %>% 
      
      addPolygons(
        data = Ortsteildaten_gesamt,
        group = "Ortsteile",
        weight = 2, 
        smoothFactor = 0.5,
        opacity = 0.5, 
        fillOpacity = 0.8, 
        color = ~binpal( Ortsteildaten_gesamt$NDVI),
        highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
        label =  lapply(labels2, HTML),
        
        
      ) %>% 
      
      
      #addLegend <- addLegend(position = "topright", pal = binpal, values = Ortsteildaten_gesamt$NDVI, title = "NDVI Farbskala") %>% 
      
      addLayersControl(
        
        
        position = "bottomright",
        overlayGroups = c("Bezirke", "Ortsteile"), 
        options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE )
      ) %>%
      
      addSearchFeatures(targetGroups  = c("Ortsteile", "Bezirke"),
                        options = searchFeaturesOptions(zoom=12, openPopup=TRUE)) %>%
    
      hideGroup("Ortsteile")
      
  })  
  

      
      observe({
        proxy <- leafletProxy("map1")

        if (input$legende) {
          proxy %>% addLegend(position = "topright", pal = binpal, values = Ortsteildaten_gesamt$NDVI, title = "NDVI Farbskala")
        }else{
          proxy %>% clearControls()
        }
     
      })   
      
      
      
      observeEvent(input$Ortsteile_umschalten{
        map1_proxy = leafletProxy("map1") %>%
          clearGroup('circles')
      }) 
      
      
      
      
      observe({
        if(input$umkreisBox){
            click <- input$map1_click
            if(is.null(click))
              return()

             # ClickLong = click$lng
              #ClickLat = click$lat

              point <- data.frame(ID=1, X = c(click$lng), Y = c(click$lat))
              coordinates(point) <- c("X", "Y")
 
              proj4string(point) <- CRS("+proj=longlat +datum=WGS84")
              point <- spTransform(point, CRS("+proj=utm +zone=32 +datum=WGS84 +ellps=WGS84")) %>% data.frame()
              
              circle <- spCircle(1000, CRS("+proj=utm +zone=32 +datum=WGS84 +ellps=WGS84"), centerPoint = c(x=point[1,2], y=point[1,3]))
              print("Berechne NDVI...")
              circle_ndvi <- compute_vegetation_index(tif_ndvi, circle$spCircle)
              #hier noch circle_ndvi auf NULL checken

              text<-paste("NDVI für angeklickten Ort mit Radius 1km: " , circle_ndvi)

             #addPolygons(map1, data = wohnumkreis)
             
  
              #clearMarkers('circles')%>%
              map1_proxy = leafletProxy("map1") %>%
                
                
              

              addCircles(click$lng, 
                         click$lat, 
                         group = 'circles', 
                         weight=1, 
                         radius=1000, 
                         color='black', 
                         fillColor = binpal( circle_ndvi), 
                         label=text, 
                         popup = text,
                         fillOpacity=0.5, 
                         layerId = NULL,
                         opacity=1)
              
        }else{
          map1_proxy = leafletProxy("map1") %>%
            clearGroup('circles')
        }
      })
      
      

}

shinyApp(ui = ui, server = server)
