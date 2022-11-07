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
skim(house)

  ####CREACIÓN DE VARIABLES ESPACIALES 

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

##para unir 
a1 <- c(osmdata_sf (q1), osmdata_sf (q2))

osm1_sf = a1%>%
  osmdata_sf()
osm1_sf
Zonassociales= osm1_sf$osm_points %>% 
  select(osm_id,amenity) 
Zonassociales

##########zona financiera ####################

q3 <- opq ("Bogotá Colombia")%>%
  add_osm_feature (
    key = "amenity",
    value = "bank")


osm3_sf= q3%>%
  osmdata_sf()
osm3_sf
bank= osm3_sf$osm_points %>% 
  select(osm_id,amenity) 
bank

##########Zonas de aprendizaje ####################

q4 <- opq ("Bogotá Colombia") %>%
  add_osm_feature(
    key = "amenity",
    value = "library"
  ) 
q5 <- opq ("Bogotá Colombia")%>%
  add_osm_feature (
    key = "amenity",
    value = "school")

##para unir 
a2 <- c(osmdata_sf (q4), osmdata_sf (q5))

osm4_sf = a2%>%
  osmdata_sf(a2)
osm4_sf
zonasdeaprendizaje= osm4_sf$osm_points %>% 
  select(osm_id,amenity) 
zonasdeaprendizaje



chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon
leaflet() %>% addTiles() %>% addPolygons(data=chapinero)


parques <- opq(bbox = getbb("UPZ Chapinero, Bogota")) %>%
  add_osm_feature(key = "amenity", value = "bank") %>%
  osmdata_sf() %>% .$osm_polygons %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addPolygons(data=parques)

