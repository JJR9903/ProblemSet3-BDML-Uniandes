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

#we need restaurants and ¿? 
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
a <- c(osmdata_sf (q1), osmdata_sf (q2))
a

z_sociales= a$osm_points %>% 
  select(osm_id,amenity) 
z_sociales

##########zona financiera ####################

q3 <- opq ("Bogotá Colombia")%>%
  add_osm_feature (
    key = "amenity",
    value = "bank")


osm3_sf= q3%>%
  osmdata_sf()
osm3_sf
z_finan= osm3_sf$osm_points %>% 
  select(osm_id,amenity) 
z_finan

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

a2 <- c(osmdata_sf (q4), osmdata_sf (q5))
a2

z_aprendizajein= a2$osm_points %>% 
  select(osm_id,amenity) 

z_aprendizajein

gc()

####Universidades####

q4 <- opq ("Medellin Colombia") opq ("Bogota Colombia")%>%
  add_osm_feature (
    key = "amenity",
    value = "university")


osm4_sf= q4%>%
  osmdata_sf()
osm4_sf
z_uni= osm4_sf$osm_points %>% 
  select(osm_id,amenity) 
z_uni


#chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
 #                  featuretype = "boundary:administrative", 
  #                 format_out = "sf_polygon") %>% .$multipolygon
#leaflet() %>% addTiles() %>% addPolygons(data=chapinero)


#parques <- opq(bbox = getbb("UPZ Chapinero, Bogota")) %>%
 # add_osm_feature(key = "amenity", value = "bank") %>%
  #osmdata_sf() %>% .$osm_polygons %>% select(osm_id,name)

#leaflet() %>% addTiles() %>% addPolygons(data=parques)

