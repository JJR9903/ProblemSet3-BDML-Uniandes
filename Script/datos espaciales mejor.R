#Problem set 3 

rm (list= ls())
library(pacman)
p_load(rstudioapi, tidyverse, sf, rio, osmdata,leaflet, skimr)
path<- dirname(getActiveDocumentContext()$path)
setwd(path) 
dir()

##Importar base 

house <- import("stores/dataPS3/train.rds")
house <- st_as_sf(x = house, ## datos
                  coords=c("lon","lat"), ## coordenadas
                  crs=4326) ## CRS

st_geometry(house)

##Descriptivas 
class(house)
#skim(house)

####CREACIÓN DE VARIABLES ESPACIALES 

#####Lista de items 
      #####Con amenity 
list <-available_tags("amenity") %>% 
  head(1000)

city<- ("Bogotá") 
zone<- c("bank", "university", "restaurant","veterinary", "pub", "marketplace", "hospital", "clinic", "pharmacy", "prison", "bar", "cafe", "college","nightclub","school","library","bus_station","kindergarten","waste_disposal")
base<-data.frame()

for (i in 1:length(zone)){
  al <- opq (paste0(city," Colombia"))%>%
    add_osm_feature (
      key = "amenity",
      value = zone[i])
  osm_al= al%>%
    osmdata_sf()
  osm_al2<-osm_al$osm_points%>% 
    select(osm_id,amenity) 
  assign(paste0("osm_al_",zone[i]),osm_al2)
  
  
}  




#####Con leisure
list <-available_tags("leisure") %>% 
  head(1000)
list
city<- ("Bogotá") 
zone2<- c("park", "fitness_centre" ,"sports_centre", "fitness_station", "garden","horse_riding", "swimming_pool" ,"swimming_area", "golfcourse", "dance" ,"stadium","dog_park", "sports_centre","disc_golf_course")
base<-data.frame()

for (i in 1:length(zone2)){
  al <- opq (paste0(city," Colombia"))%>%
    add_osm_feature (
      key = "amenity",
      value = zone2[i])
  osm_al= al%>%
    osmdata_sf()
  osm_al2<-osm_al$osm_points%>% 
    select(osm_id,amenity) 
  assign(paste0("osm_al_",zone[i]),osm_al2)
  
  
}  


