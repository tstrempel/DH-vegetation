if (!require("pacman")) install.packages("pacman")
pacman::p_load(leaflet, leaflet.extras, rgdal, shiny, rsconnect, htmltools, RColorBrewer, dplyr, sampSurf)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rgdal_show_exportToProj4_warnings = "none"

source("VegetationIndex.R")
tif_ndvi <- raster("data/NDVI.tif")


# first read in shapefiles of ortsteile and bezirke; then read in NDVI, EVI and demographical data, add to data set:

#bezirke:
leipzig_Bezirke <- readOGR(dsn = "./shapefiles/Leipzig_Stadtbezirke_UTM33N", layer = "sbz", use_iconv = TRUE, encoding = "UTF-8") #leipzig_Bezirke is geospatial object
leipzig_Bezirke <- spTransform(leipzig_Bezirke, CRS("+proj=longlat +datum=WGS84"))

Bezirke_NDVI <- read.csv(file = './data/sbz_ndvi.csv', row.names=NULL, encoding = "UTF-8")
Bezirke_EVI <- read.csv(file = './data/sbz_evi.csv', row.names=NULL, encoding = "UTF-8")
Bezirke_Einkommen <- read.csv(file = './data/Einkommen_und_Preise_Nettoeinkommen_SBZ.csv', sep=";", dec=",", row.names=NULL, encoding = "UTF-8" )

Bezirksdaten_gesamt <- merge(leipzig_Bezirke, Bezirke_NDVI )
Bezirksdaten_gesamt <- merge(Bezirksdaten_gesamt, Bezirke_EVI) 
Bezirksdaten_gesamt <- merge(Bezirksdaten_gesamt, Bezirke_Einkommen)#complete data set Bezirksebene
Bezirksdaten_gesamt <- Bezirksdaten_gesamt[order(-Bezirksdaten_gesamt$Einkommen),]
Bezirksdaten_gesamt$X <- 1:nrow(Bezirksdaten_gesamt)
Bezirksdaten_gesamt #complete data set Bezirksebene, sortet by income 




#ortsteile:
leipzig_Ortsteile = readOGR(dsn = "./shapefiles/Leipzig_Ortsteile_UTM33N", layer = "ot", use_iconv = TRUE, encoding = "UTF-8") #leipzig_Ortsteile is geospatial object
leipzig_Ortsteile <- spTransform(leipzig_Ortsteile, CRS("+proj=longlat +datum=WGS84"))

OT_NDVI <- read.csv(file = './data/ot_ndvi.csv', row.names=NULL, encoding = "UTF-8")
OT_EVI <- read.csv(file = './data/ot_evi.csv', row.names=NULL, encoding = "UTF-8")
OT_Einkommen <- read.csv(file = './data/Einkommen_und_Preise_Nettoeinkommen_OT.csv', sep=";", dec=",", row.names=NULL, encoding = "UTF-8" )

Ortsteildaten_gesamt <- merge(leipzig_Ortsteile, OT_NDVI )
Ortsteildaten_gesamt <- merge(Ortsteildaten_gesamt, OT_EVI) 
Ortsteildaten_gesamt <- merge(Ortsteildaten_gesamt, OT_Einkommen) #complete data set Ortsteilebene
Ortsteildaten_gesamt <- Ortsteildaten_gesamt[order(-Ortsteildaten_gesamt$Einkommen),]
Ortsteildaten_gesamt$X <- 1:nrow(Ortsteildaten_gesamt)
Ortsteildaten_gesamt #complete data set Ortsteilebene, sortet by income 

#using leaflet and shiny to create interactive map

#first define some labels and colors using html:

labels <- paste("<p>", "Bezirk: ", Bezirksdaten_gesamt$Name,  "</p>",
                "<p>", "NDVI: ", round(Bezirksdaten_gesamt$NDVI, digits = 3) , "</p>",
                "<p>", "EVI: ", round(Bezirksdaten_gesamt$EVI, digits = 3) , "</p>",
                "<p>", "Median income: ", round(Bezirksdaten_gesamt$Einkommen, digits = 2) , " Euro. (Rank " , Bezirksdaten_gesamt$X , " of ", nrow(Bezirksdaten_gesamt) ,")", "</p>",
                sep= "")

binpal <- colorBin("Greens", Ortsteildaten_gesamt$NDVI, n = 7)

labels2 <- paste("<p>", "Ortsteil: ", Ortsteildaten_gesamt$Name,  "</p>",
                "<p>", "NDVI: ", round(Ortsteildaten_gesamt$NDVI, digits = 3) , "</p>",
                "<p>", "EVI: ", round(Ortsteildaten_gesamt$EVI, digits = 3) , "</p>",
                "<p>", "Median income: ", round(Ortsteildaten_gesamt$Einkommen, digits = 2) , " Euro. (Rank " , Ortsteildaten_gesamt$X , " of ", nrow(Ortsteildaten_gesamt) ,")", "</p>",
                
                sep= "")


#and now the shiny application:

ui <- fillPage(
  leafletOutput("map1", height = "95%"),
  
  absolutePanel(top = 2, right = 2,
    sliderInput(
      "radiusslider", "Select radius (in meters)", min = 100, max = 5000, value = 1000, step = 100
    )  ,
    #actionButton("Umkreismodus", "Ortsteile zeigen"),
    checkboxInput("legende", "show color scale", TRUE),
    checkboxInput("umkreisBox", "calculate NDVI for my area of residence", FALSE)
  
  ),
  titlePanel(windowTitle = "Green spaces Leipzig visualization", title = "Green spaces Leipzig visualization"),
  
    mainPanel(
      
      title = "Green spaces Leipzig"

           
    )
    )

server <- function(input, output, session){
  output$map1 <- renderLeaflet({
    
    
    leaflet(options = leafletOptions(minZoom = 9, maxZoom = 15), Bezirksdaten_gesamt) %>%
    addTiles() %>% 
    setView(lat=51.34, lng=12.36, zoom=12) %>% 
    addPolygons(
        data = Bezirksdaten_gesamt,
        group = "Bezirke",
        weight = 2, 
        smoothFactor = 0.5,
        opacity = 0.5, 
        fillOpacity = 0.8, 
        color = ~binpal( Bezirksdaten_gesamt$NDVI),
        highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
        label =  lapply(labels, HTML)
        #layerId = ~Name #to identify which shape is clicked
        
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
        label =  lapply(labels2, HTML)
        
      ) %>% 
      
      addLayersControl(
        position = "bottomleft",
        overlayGroups = c("Bezirke", "Ortsteile"), 
        options = layersControlOptions(collapsed = F, autoZIndex = F )
        
      ) %>%
      
      addSearchFeatures(
        targetGroups  = c("Ortsteile", "Bezirke"),
        options = searchFeaturesOptions(zoom=12, openPopup=TRUE)) %>%
        hideGroup("Ortsteile") 
      
  })  
  
  

  
  observe({
    proxy <- leafletProxy("map1")
    
      if (input$legende) {
        proxy %>% addLegend(position = "bottomright", pal = binpal, values = Ortsteildaten_gesamt$NDVI, title = "NDVI color scale")
      }else{
        proxy %>% clearControls()
      }
    
  })   
  

  observeEvent(input$map1_groups,{
    proxy <- leafletProxy("map1") 
    if (input$map1_groups == 'Ortsteile') {
      proxy %>% hideGroup("Bezirke")
    }
    else if (input$map1_groups == 'Bezirke') {
      proxy %>% hideGroup("Ortsteile")
    }
  })
  
  
  
  observeEvent(input$Umkreismodus, {
    map1_proxy = leafletProxy("map1") %>%
      clearGroup('circles') %>%
      hideGroup("Ortsteile") %>%
      hideGroup("Bezirke")
  }) 

  
  observe({
    if(input$umkreisBox){
      map1_proxy = leafletProxy("map1") %>%
        hideGroup("Ortsteile") %>%
        hideGroup("Bezirke")

      
      click <- input$map1_click
      if(is.null(click))
        return()
      
      point <- data.frame(ID=1, X = c(click$lng), Y = c(click$lat))
      coordinates(point) <- c("X", "Y")
      
      proj4string(point) <- CRS("+proj=longlat +datum=WGS84")
      point <- spTransform(point, CRS("+proj=utm +zone=32 +datum=WGS84 +ellps=WGS84")) %>% data.frame()
      
      circle <- spCircle(input$radiusslider, CRS("+proj=utm +zone=32 +datum=WGS84 +ellps=WGS84"), centerPoint = c(x=point[1,2], y=point[1,3]))
      print("Berechne NDVI...")
      circle_ndvi <- compute_vegetation_index(tif_ndvi, circle$spCircle)
      #hier noch circle_ndvi auf NULL checken
      
      text<-paste("NDVI for selected radius: " , round(circle_ndvi, 3))
      
      
      map1_proxy = leafletProxy("map1") %>%
        
        addCircles(click$lng, 
                   click$lat, 
                   group = 'circles', 
                   weight=1, 
                   radius=input$radiusslider, 
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

