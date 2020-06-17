# Read this shape file with the rgdal library. 
library(gpclib)
library(rgeos)
library(rgdal)
library(broom)
library(ggplot2)
library(dplyr)
library(leaflet)
library(RColorBrewer)
world_spdf <- readOGR( 
  dsn=path.expand("/Users/xnxxxnxx/Desktop/5147/assignment/visualization/world_shape_file/"), 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

initialData <- read.csv("~/Desktop/5147/assignment/visualization/map2015.csv")
print(initialData$PERSONAL)
#initialData$location <- factor(initialData$location,levels=unique(initialData$location[order(initialData$latitude)]),ordered = TRUE)
initialData$PERSONAL <-as.numeric(as.character(initialData$PERSONAL))/1 %>% round(2)

#world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
#world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)
#print(world_spdf@data)
# Create a color palette with handmade bins.
mybins <- c(2,3,4,5,6,7,Inf)
mypalette <- colorBin( palette="RdBu", domain=initialData$PERSONAL, na.color="transparent", bins=mybins)

# Prepare the text for tooltips:
mytext <- paste(
  "Country: ", initialData$Country,"<br/>", 
  "Happiness: ", round(initialData$PERSONAL, 2), 
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map
leaflet(world_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor =~mypalette(initialData$PERSONAL), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~initialData$PERSONAL, opacity=0.9, title = "Happiness Value", position = "bottomleft" )





#