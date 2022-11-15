
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
p_load(rstudioapi, tidyverse, sf, rio, osmdata,leaflet, skimr)
p_load(tidyverse,rio,skimr,viridis,osmdata,
       ggsn, ## scale bar
       raster,stars, ## datos raster
       ggmap, ## get_stamenmap
       sf, ## Leer/escribir/manipular datos espaciales
       leaflet) ## Visualizaciones dinámicas
path<- dirname(getActiveDocumentContext()$path)
setwd(path) 
dir()


####CREACIÓN DE VARIABLES ESPACIALES 

#####Lista de items 
#####Con amenity 
list <-available_tags("amenity") %>% 
  head(1000)

city<- ("Cali") 
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
    dplyr::select(osm_id,amenity) 
  assign(paste0("osm_al_",zone[i]),osm_al2)
}  

#####Con leisure
list <-available_tags("leisure") %>% 
  head(1000)
list
city<- ("Cali") 
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
    dplyr::select(osm_id,leisure) 
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
osm_al_veterinary<-osm_al_veterinary%>%
  mutate(amenity="Pet")

#Zona verde
#osm_al_park<-osm_al_park%>%
#mutate(amenity="park")
osm_al_garden<-osm_al_garden%>%
  mutate(amenity="park")

#Fitness
osm_al_fitness_centre<-osm_al_fitness_centre%>%
  mutate(amenity="fitness")
osm_al_fitness_station<-osm_al_fitness_station%>%
  mutate(amenity="fitness")
osm_al_sports_centre<-osm_al_sports_centre%>%
  mutate(amenity="fitness")

####sacar barrios de Cali
Cali<-st_read(file.path("stores/Cali/barrios_catastrales/barrios_catastrales.shp"))%>%
  st_transform(crs = 4326)

####### Poner barrios a la base 
osm_al_bank <- st_intersection(x = osm_al_bank , y = Cali)
osm_al_cafe <- st_intersection(x = osm_al_cafe , y = Cali)
osm_al_clinic <- st_intersection(x = osm_al_clinic , y = Cali)
osm_al_college <- st_intersection(x = osm_al_college , y = Cali)
osm_al_fitness_centre <- st_intersection(x = osm_al_fitness_station , y = Cali)
osm_al_fitness_station <- st_intersection(x = osm_al_fitness_station , y = Cali)
osm_al_garden <- st_intersection(x = osm_al_garden , y = Cali)
osm_al_hospital <- st_intersection(x = osm_al_hospital , y = Cali)
osm_al_kindergarten <- st_intersection(x = osm_al_kindergarten, y = Cali)
osm_al_library <- st_intersection(x = osm_al_library, y = Cali)
osm_al_marketplace <- st_intersection(x = osm_al_marketplace , y = Cali)
osm_al_nightclub <- st_intersection(x = osm_al_nightclub , y = Cali)
#osm_al_park <- st_intersection(x = osm_al_park, y = Cali)
osm_al_pharmacy <- st_intersection(x = osm_al_pharmacy, y = Cali)
osm_al_prison <- st_intersection(x = osm_al_prison, y = Cali)
osm_al_pub <- st_intersection(x = osm_al_pub, y = Cali)
osm_al_restaurant <- st_intersection(x = osm_al_restaurant, y = Cali)
osm_al_school <- st_intersection(x = osm_al_school, y = Cali)
osm_al_sports_centre <- st_intersection(x = osm_al_sports_centre, y = Cali)
osm_al_stadium <- st_intersection(x = osm_al_stadium , y = Cali)
osm_al_swimming_area <- st_intersection(x = osm_al_swimming_area, y = Cali)
osm_al_swimming_pool <- st_intersection(x = osm_al_swimming_pool, y = Cali)
osm_al_university <- st_intersection(x = osm_al_university, y = Cali)
osm_al_veterinary <- st_intersection(x = osm_al_veterinary, y = Cali)
osm_al_waste_disposal<- st_intersection(x = osm_al_waste_disposal, y = Cali)

#Pegado de bases no eficiente pero util 

B_Cali<-bind_rows(osm_al_bank,osm_al_bar)
B_Cali<-bind_rows(B_Cali,osm_al_bus_station)
B_Cali<-bind_rows(B_Cali,osm_al_cafe)
B_Cali<-bind_rows(B_Cali,osm_al_clinic)
B_Cali<-bind_rows(B_Cali,osm_al_college)
B_Cali<-bind_rows(B_Cali,osm_al_fitness_centre)
B_Cali<-bind_rows(B_Cali,osm_al_fitness_station)
B_Cali<-bind_rows(B_Cali,osm_al_garden)
B_Cali<-bind_rows(B_Cali,osm_al_hospital)
B_Cali<-bind_rows(B_Cali,osm_al_kindergarten)
B_Cali<-bind_rows(B_Cali,osm_al_library)
B_Cali<-bind_rows(B_Cali,osm_al_marketplace)
B_Cali<-bind_rows(B_Cali,osm_al_nightclub)
#B_Cali<-bind_rows(B_Cali,osm_al_park)
B_Cali<-bind_rows(B_Cali,osm_al_pharmacy)
B_Cali<-bind_rows(B_Cali,osm_al_prison)
B_Cali<-bind_rows(B_Cali,osm_al_pub)
B_Cali<-bind_rows(B_Cali,osm_al_restaurant)
B_Cali<-bind_rows(B_Cali,osm_al_school)
B_Cali<-bind_rows(B_Cali,osm_al_sports_centre)
B_Cali<-bind_rows(B_Cali,osm_al_stadium)
B_Cali<-bind_rows(B_Cali,osm_al_swimming_area)
B_Cali<-bind_rows(B_Cali,osm_al_swimming_pool)
B_Cali<-bind_rows(B_Cali,osm_al_university)
B_Cali<-bind_rows(B_Cali,osm_al_veterinary)
B_Cali<-bind_rows(B_Cali,osm_al_waste_disposal)

#Una variable que sea de City 

B_Cali$city<-"Cali"

#rename vars 
rename (B_Cali, CODIGO=BAR_ID)
rename (B_Cali, NOMBRE=NOMB_BARR)

#Guardar base solo Cali 

saveRDS(B_Cali,file=paste0(getwd(),"/stores/Cali.rds"))
