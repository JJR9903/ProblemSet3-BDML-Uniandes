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
library(pacman)

#p_load(ggmap,stars,raster,ggsn,rstudioapi, tidyverse, sf, rio, osmdata,leaflet, skimr,tidyverse,rio,skimr,viridis,osmdata)

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
zone2<- c("park", "fitness_centre" ,"sports_centre", "fitness_station", "garden", "swimming_pool" ,"swimming_area", "stadium", "dog_park", "sports_centre")
base<-data.frame()

for (i in 1:length(zone2)){
  al <- opq (paste0(city," Colombia"))%>%
    add_osm_feature (
      key = "leisure",
      value = zone2[i])
  osm_al= al%>%
    osmdata_sf()
  osm_al2<-osm_al$osm_points%>% 
    select(osm_id,leisure) 
  assign(paste0("osm_al_",zone2[i]),osm_al2)
}  

##Arreglar bases
#bank 
osm_al_bar<-osm_al_bank%>%
  mutate(amenity="bank")
#comida
osm_al_bar<-osm_al_bar%>%
  mutate(amenity="comida")
osm_al_cafe<-osm_al_cafe%>%
  mutate(amenity="comida")
osm_al_pub<-osm_al_pub%>%
  mutate(amenity="comida")
osm_al_restaurant<-osm_al_restaurant%>%
  mutate(amenity="comida")

#salud
osm_al_clinic<-osm_al_clinic%>%
  mutate(amenity="salud")
osm_al_hospital<-osm_al_hospital%>%
  mutate(amenity="salud")
osm_al_pharmacy<-osm_al_pharmacy%>%
  mutate(amenity="salud")

#educacion superior
osm_al_college<-osm_al_college%>%
  mutate(amenity="E Superior")
osm_al_university<-osm_al_university%>%
  mutate(amenity="E Superior")

#educación 
osm_al_kindergarten<-osm_al_kindergarten%>%
  mutate(amenity="Educacion")
osm_al_school<-osm_al_school%>%
  mutate(amenity="Educacion")
osm_al_library<-osm_al_library%>%
  mutate(amenity="Educacion")

#depre
#nignt club 
osm_al_nightclub<-osm_al_nightclub%>%
  mutate(amenity="nightclub")

#Prision 
osm_al_prison<-osm_al_prison%>%
  mutate(amenity="prison")

#stadium 
osm_al_stadium<-osm_al_stadium%>%
  mutate(amenity="stadium")

#waste disposal 
osm_al_waste_disposal<-osm_al_waste_disposal%>%
  mutate(amenity="waste_disposal")

#Swimming
osm_al_swimming_area<-osm_al_swimming_area%>%
  mutate(amenity="swimming")
osm_al_swimming_pool<-osm_al_swimming_pool%>%
  mutate(amenity="swimming")

#Market place 
osm_al_marketplace<-osm_al_marketplace%>%
  mutate(amenity="marketplace")

#Estaciones de bus 
osm_al_bus_station<-osm_al_bus_station%>%
  mutate(amenity="bus_station")

#Pet
osm_al_dog_park<-osm_al_dog_park%>%
  mutate(amenity="Pet")
osm_al_veterinary<-osm_al_veterinary%>%
  mutate(amenity="Pet")

#Zona verde
osm_al_park<-osm_al_park%>%
  mutate(amenity="park")
osm_al_garden<-osm_al_garden%>%
  mutate(amenity="park")

#Fitness
osm_al_fitness_centre<-osm_al_fitness_centre%>%
  mutate(amenity="fitness")
osm_al_fitness_station<-osm_al_fitness_station%>%
  mutate(amenity="fitness")
osm_al_sports_centre<-osm_al_sports_centre%>%
  mutate(amenity="fitness")

#Pegado de bases no eficiente pero util 

B_Bogota<-bind_rows(osm_al_bank,osm_al_bar)
B_Bogota<-bind_rows(B_Bogota,osm_al_bus_station)
B_Bogota<-bind_rows(B_Bogota,osm_al_cafe)
B_Bogota<-bind_rows(B_Bogota,osm_al_clinic)
B_Bogota<-bind_rows(B_Bogota,osm_al_college)
B_Bogota<-bind_rows(B_Bogota,osm_al_dog_park)
B_Bogota<-bind_rows(B_Bogota,osm_al_fitness_centre)
B_Bogota<-bind_rows(B_Bogota,osm_al_fitness_station)
B_Bogota<-bind_rows(B_Bogota,osm_al_garden)
B_Bogota<-bind_rows(B_Bogota,osm_al_hospital)
B_Bogota<-bind_rows(B_Bogota,osm_al_kindergarten)
B_Bogota<-bind_rows(B_Bogota,osm_al_library)
B_Bogota<-bind_rows(B_Bogota,osm_al_marketplace)
B_Bogota<-bind_rows(B_Bogota,osm_al_nightclub)
B_Bogota<-bind_rows(B_Bogota,osm_al_park)
B_Bogota<-bind_rows(B_Bogota,osm_al_pharmacy)
B_Bogota<-bind_rows(B_Bogota,osm_al_prison)
B_Bogota<-bind_rows(B_Bogota,osm_al_pub)
B_Bogota<-bind_rows(B_Bogota,osm_al_restaurant)
B_Bogota<-bind_rows(B_Bogota,osm_al_school)
B_Bogota<-bind_rows(B_Bogota,osm_al_sports_centre)
B_Bogota<-bind_rows(B_Bogota,osm_al_stadium)
B_Bogota<-bind_rows(B_Bogota,osm_al_swimming_area)
B_Bogota<-bind_rows(B_Bogota,osm_al_swimming_pool)
B_Bogota<-bind_rows(B_Bogota,osm_al_university)
B_Bogota<-bind_rows(B_Bogota,osm_al_veterinary)
B_Bogota<-bind_rows(B_Bogota,osm_al_waste_disposal)

#Una variable que sea de City 

B_Bogota$city<-"bogota"

#Guardar base solo bogotá 

saveRDS(B_Bogota,file=paste0(getwd(),"/stores/Bogotá.rds"))

#######Sacar los "barrios" de cada cosa 
#######Leer base y ponerle barrios 
####### Ver lo de las distancias 


####MEDELLIN####
city<- ("Medellín") 
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
zone2<- c("park", "fitness_centre" ,"sports_centre", "fitness_station", "garden", "swimming_pool", "dog_park", "sports_centre")
base<-data.frame()

for (i in 1:length(zone2)){
  al <- opq (paste0(city," Colombia"))%>%
    add_osm_feature (
      key = "leisure",
      value = zone2[i])
  osm_al= al%>%
    osmdata_sf()
  osm_al2<-osm_al$osm_points%>% 
    select(osm_id,leisure) 
  assign(paste0("osm_al_",zone2[i]),osm_al2)
}  

##Arreglar bases
#bank 
osm_al_bar<-osm_al_bank%>%
  mutate(amenity="bank")
#comida
osm_al_bar<-osm_al_bar%>%
  mutate(amenity="comida")
osm_al_cafe<-osm_al_cafe%>%
  mutate(amenity="comida")
osm_al_pub<-osm_al_pub%>%
  mutate(amenity="comida")
osm_al_restaurant<-osm_al_restaurant%>%
  mutate(amenity="comida")
#salud
osm_al_clinic<-osm_al_clinic%>%
  mutate(amenity="salud")
osm_al_hospital<-osm_al_hospital%>%
  mutate(amenity="salud")
osm_al_pharmacy<-osm_al_pharmacy%>%
  mutate(amenity="salud")
#educacion superior
osm_al_college<-osm_al_college%>%
  mutate(amenity="E Superior")
osm_al_university<-osm_al_university%>%
  mutate(amenity="E Superior")
#educación 
osm_al_kindergarten<-osm_al_kindergarten%>%
  mutate(amenity="Educacion")
osm_al_school<-osm_al_school%>%
  mutate(amenity="Educacion")
osm_al_library<-osm_al_library%>%
  mutate(amenity="Educacion")
#depre
osm_al_nightclub<-osm_al_nightclub%>%
  mutate(amenity="nightclub")
osm_al_prison<-osm_al_prison%>%
  mutate(amenity="prison")
osm_al_waste_disposal<-osm_al_waste_disposal%>%
  mutate(amenity="waste_disposal")


#Swimming
osm_al_swimming_pool<-osm_al_swimming_pool%>%
  mutate(amenity="swimming")


osm_al_marketplace<-osm_al_marketplace%>%
  mutate(amenity="marketplace")


osm_al_bus_station<-osm_al_bus_station%>%
  mutate(amenity="bus_station")
#Pet
osm_al_dog_park<-osm_al_dog_park%>%
  mutate(amenity="Pet")
osm_al_veterinary<-osm_al_veterinary%>%
  mutate(amenity="Pet")
#Zona verde
osm_al_park<-osm_al_park%>%
  mutate(amenity="park")
osm_al_garden<-osm_al_garden%>%
  mutate(amenity="park")
#Fitness
osm_al_fitness_centre<-osm_al_fitness_centre%>%
  mutate(amenity="fitness")
osm_al_fitness_station<-osm_al_fitness_station%>%
  mutate(amenity="fitness")
osm_al_sports_centre<-osm_al_sports_centre%>%
  mutate(amenity="fitness")

B_Medellin<-bind_rows(osm_al_bank,osm_al_bar)
B_Medellin<-bind_rows(B_Medellin,osm_al_bus_station)
B_Medellin<-bind_rows(B_Medellin,osm_al_cafe)
B_Medellin<-bind_rows(B_Medellin,osm_al_clinic)
B_Medellin<-bind_rows(B_Medellin,osm_al_college)
B_Medellin<-bind_rows(B_Medellin,osm_al_dog_park)
B_Medellin<-bind_rows(B_Medellin,osm_al_fitness_centre)
B_Medellin<-bind_rows(B_Medellin,osm_al_fitness_station)
B_Medellin<-bind_rows(B_Medellin,osm_al_garden)
B_Medellin<-bind_rows(B_Medellin,osm_al_hospital)
B_Medellin<-bind_rows(B_Medellin,osm_al_kindergarten)
B_Medellin<-bind_rows(B_Medellin,osm_al_library)
B_Medellin<-bind_rows(B_Medellin,osm_al_marketplace)
B_Medellin<-bind_rows(B_Medellin,osm_al_nightclub)
B_Medellin<-bind_rows(B_Medellin,osm_al_park)
B_Medellin<-bind_rows(B_Medellin,osm_al_pharmacy)
B_Medellin<-bind_rows(B_Medellin,osm_al_prison)
B_Medellin<-bind_rows(B_Medellin,osm_al_pub)
B_Medellin<-bind_rows(B_Medellin,osm_al_restaurant)
B_Medellin<-bind_rows(B_Medellin,osm_al_school)
B_Medellin<-bind_rows(B_Medellin,osm_al_sports_centre)
B_Medellin<-bind_rows(B_Medellin,osm_al_swimming_pool)
B_Medellin<-bind_rows(B_Medellin,osm_al_university)
B_Medellin<-bind_rows(B_Medellin,osm_al_veterinary)
B_Medellin<-bind_rows(B_Medellin,osm_al_waste_disposal)

B_Medellin$city<-"medellin"

rename(B_Medellin,  leisure=amenity)

saveRDS(B_Medellin,file=paste0(getwd(),"/stores/medellin.rds"))

####MERGE FINAL#### :3

B_MB<-bind_rows(B_Medellin,B_Bogota)

#B_MB<-B_MB%>%
#mutate(amenity=case_when(is.na(amenity)==TRUE~ leisure),
                       # FALSE~amenity)

saveRDS(B_MB,file=paste0(getwd(),"/stores/espacial_mb.rds"))

