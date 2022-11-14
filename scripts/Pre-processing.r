## Autores: Juan José Rincón , Juan Andres Ospina, Juanita Chacón 
## Descripción: Desarrollo 3 problem set /Big Data and Machine Leanring for applied economics
## Universidad de los Andes 2022-2
## Creation Date: 26/10/2022
####################################

#### setting the work space 

rm(list=ls())

dir_set <- function(){
  if(Sys.info()["user"]=="JuanJose"){
    setwd("/Users/JuanJose/Library/CloudStorage/OneDrive-UniversidaddelosAndes/Uniandes/9 Semestre - 1 PEG/Big Data/Problems Set/ProblemSet3-BDML-Uniandes")
  }
  else if(Sys.info()["user"]=="PC-PORTATIL"){
    setwd("C:/Users/PC-PORTATIL/OneDrive/Documentos/GitHub/ProblemSet3-BDML-Uniandes")
  }
  else{
    setwd("C:/Users/ja.ospinap/Downloads/ProblemSet3-BDML-Uniandes")
  }
}


dir_set()

pacman:: p_load(tidyverse, skimr, fastDummies, sf, leaflet, osmdata, tmaptools)


leaflet() %>%
  addTiles()

casa_narino<-geocode_OSM("casa de nariño, Bogotá",as.sf=T)
class(casa_narino)
casa_narino


leaflet() %>%
  addTiles()%>%
  addCircles(data = casa_narino)

available_features()

available_features()
  
available_tags("amenity")

# restaurant y bar, cafe, fast_food, 

# university

#bank ? 

#clinic

#police

# marketplace (galerias)
Cali_Comida<-opq(bbox=getbb("Cali, Colombia"))%>%
  add_osm_feature(key = "amenity", value = c("restaurant","bar","cafe","fast_food")) %>% 
  osmdata_sf()%>%
  .$osm_points %>% 
  select(osm_id,geometry)

leaflet()%>%
  addTiles()%>%
  addCircles(data= Cali_Comida, color = "blue", radius = 1)

Cali_U<-opq(bbox=getbb("Cali, Colombia"))%>%
  add_osm_feature(key = "amenity", value = c("university")) %>% 
  osmdata_sf()%>%
  .$osm_points %>% 
  select(osm_id,geometry)

leaflet()%>%
  addTiles()%>%
  addCircles(data= Cali_U, color = "blue")


Cali_BanksOfiice<-opq(bbox=getbb("Cali, Colombia"))%>%
  add_osm_feature(key = "amenity", value = c("bank")) %>% 
  osmdata_sf()%>%
  .$osm_points %>% 
  select(osm_id,geometry)

leaflet()%>%
  addTiles()%>%
  addCircles(data= Cali_BanksOfiice, color = "blue", radius = 1)


Cali_Seguridad<-opq(bbox=getbb("Cali, Colombia"))%>%
  add_osm_feature(key = "amenity", value = c("police","clinic")) %>% 
  osmdata_sf()%>%
  .$osm_points %>% 
  select(osm_id,geometry)

leaflet()%>%
  addTiles()%>%
  addCircles(data= Cali_Seguridad, color = "blue", radius = 1)

##parques 
# leisure - park
# leisure - dog_park

# building - hotel
# building - commercial
#landuse - commercial
# building - retail
#landuse - retail
# shop
# building - supermarket

# office
# building - office

# building - industrial
#landuse - industrial


# building - warehouse

# building - stadium
#landuse - recreation_ground
#landuse -sports_centre
#landuse -stadium
#sport

#landuse - farmland
#landuse - meadow
#landuse - grass

#tourism
Cali<-opq(bbox=getbb("Bogotá, Colombia"))%>%
  add_osm_feature(key = "boundary")%>%
  osmdata_sf()

Cali_Bus<-Cali$osm_points %>% 
  select(osm_id,geometry)


leaflet()%>%
  addTiles()%>%
  addCircles(data= Cali_Bus, color = "blue")
addPolygons(data= Cali_Bus, color = "blue")
 
vignette("sf3")











#####
library(rgdal)
shape<-read_sf('stores/sector_shp/SECTOR.shp')
class(shape)
shape<-shape%>%
  subset(select=c("SCACODIGO","SCANOMBRE","geometry"))

shape<- shape%>%
  st_as_sf(x=shape, coords="geometry", crs=4326)

leaflet() %>% addTiles() %>% addPolygons(data=shape) 

str(shape, max.level = 2,give.attr = F)




############### BASE DE DATOS #######
Train <- readRDS("stores/dataPS3/train.Rds")

Test <- readRDS("stores/dataPS3/test.Rds")

skim(Test)

Cali_houses<-st_as_sf(x=Test, coords = c("lon","lat"),crs=4326)

leaflet() %>% addTiles() %>% addCircleMarkers(data=Cali_houses,radius = 0.2)

Bogota_houses<-Train%>%
  filter(Train$city=="Bogotá D.C")%>%
  st_as_sf(coords = c("lon","lat"),crs=4326)

leaflet() %>% addTiles() %>% addCircleMarkers(data=Bogota_houses,radius = 0.2)

Medellin_houses<-Train%>%
  filter(Train$city=="Medellín")%>%
  st_as_sf(coords = c("lon","lat"),crs=4326)
leaflet() %>% addTiles() %>% addCircleMarkers(data=Medellin_houses,radius = 0.2)

