rm (list= ls())
library(pacman)
p_load(rstudioapi, tidyverse, sf, rio, osmdata,leaflet, skimr)
path<- dirname(getActiveDocumentContext()$path)
setwd(path) 
dir()

house <- import("stores/dataPS3/train.rds")
class(house)
skim(house)
house <- st_as_sf(x = house, ## datos
                  coords=c("lon","lat"), ## coordenadas
                  crs=4326) ## CRS

st_geometry(house)


#####Lista de items 
list <-available_tags("amenity") %>% 
  head(1000)
list

#we neeed restaurants and ¿? 
q1 <- opq ("Bogotá Colombia") %>%
  add_osm_feature(
    key = "amenity",
    value = "restaurant"
  ) 
q2 <- opq ("Bogotá Colombia")%>%
add_osm_feature (
   key = "amenity",
    value = "pub")



osm_sf = q1%>%
  osmdata_sf()
osm_sf
socialzone= osm_sf$osm_points %>% 
  select(osm_id,amenity) 
socialzone


