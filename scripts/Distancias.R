## Autores: Juan José Rincón , Juan Andres Ospina, Juanita Chacón 
## Descripción: Desarrollo 3 problem set /Big Data and Machine Leanring for applied economics
## Universidad de los Andes 2022-2
## Creation Date: 14/09/2022
#### setting the work space ####################################

rm(list=ls())

# este setwd por ahora no esta funcionando, toca que cada uno lo haga independiente mientras logro que funcione. att: Juan Jose
dir_set <- function(){
  if(Sys.info()["user"]=="JuanJose"){
    setwd("/Users/JuanJose/Library/CloudStorage/OneDrive-UniversidaddelosAndes/Uniandes/9 Semestre - 1 PEG/Big Data/Problems Set/ProblemSet3-BDML-Uniandes")
  }
  else if(Sys.info()["user"]=="PC-PORTATIL"){
    setwd("C:/Users/PC-PORTATIL/OneDrive/Documentos/GitHub/ProblemSet3-BDML-Uniandes")
  }
  else if(Sys.info()["user"]=="juan.rincon"){
    setwd("C:/Users/juan.rincon/OneDrive - Universidad de los Andes/Uniandes/9 Semestre - 1 PEG/Big Data/Problems Set/ProblemSet3-BDML-Uniandes")
  }
  else{
    setwd("C:/Users/Usuario/Documents/GitHub/ProblemSet3-BDML-Uniandes")
  }
}

dir_set()

pacman:: p_load(tidyverse, skimr, fastDummies, sf, leaflet, osmdata, tmaptools,spdep,nngeo)
pacman::p_load(parallel,doParallel)

set.seed(1234)
n_cores<-detectCores()
cl <- makePSOCKcluster(n_cores - 1) 
registerDoParallel(cl)

#### cargar las viviendas 

Train <- readRDS("stores/Train_Barrios_manzanas_texto.rds")%>%
  st_as_sf(coords=c("lon","lat"), crs=4326) 

Test <- readRDS("stores/Test_Barrios_manzanas_texto.rds")%>%
  st_as_sf(coords=c("lon","lat"), crs=4326) 


##### cargar los amenities 

Amenities_Med <- readRDS("stores/Medellín_osm.rds")%>%
  st_as_sf(coords=c("lon","lat"), crs=4326) 

Amenities_Bog <- readRDS("stores/Bogotá_osm.rds")%>%
  st_as_sf(coords=c("lon","lat"), crs=4326) 

Amenities_Clo<- readRDS("stores/Cali_osm.rds")%>%
  st_as_sf(coords=c("lon","lat"), crs=4326) 


##### Cargar los CBDs por ciudad 


CBD_Clo <- geocode_OSM("CAM, Cali, Colombia", as.sf=T)

CBD_Med <- geocode_OSM("Centro Administrativo La Alpujarra, Medellín, Colombia", as.sf=T) 
CBD_Med <- rbind(CBD_Med,geocode_OSM("Milla de oro, Medellín, Colombia", as.sf=T) )

CBD_Bog <- geocode_OSM("Centro Internacional, Bogotá, Colombia", as.sf=T) 
CBD_Bog <- rbind(CBD_Bog,geocode_OSM("central avenida chile, Bogotá, Colombia", as.sf=T))
CBD_Bog <- rbind(CBD_Bog,geocode_OSM("World Trade Center, Bogotá, Colombia", as.sf=T) )
CBD_Bog <- rbind(CBD_Bog,geocode_OSM("Teleport, Bogotá, Colombia", as.sf=T) )
CBD_Bog <- rbind(CBD_Bog,geocode_OSM("NH collection royal andino, Bogotá, Colombia", as.sf=T) )
CBD_Bog <- rbind(CBD_Bog,geocode_OSM("Parque de la 93, Bogotá, Colombia", as.sf=T) )
CBD_Bog <- rbind(CBD_Bog,geocode_OSM("Ciudad Empresarial Sarmiento Angulo, Bogotá, Colombia", as.sf=T) )



###################### distancias a amenities ##################################

Test<-Test%>%
  mutate(NearBank=st_nn(Test,Amenities_Clo[Amenities_Clo$feature_type=="bank",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearpolice=st_nn(Test,Amenities_Clo[Amenities_Clo$feature_type=="police",],returnDist = TRUE, parallel = 7)[["dist"]],
         NearComida=st_nn(Test,Amenities_Clo[Amenities_Clo$feature_type=="Comida",],returnDist = TRUE, parallel = 7)[["dist"]],
         NearES=st_nn(Test,Amenities_Clo[Amenities_Clo$feature_type=="educacion superior",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearveterinary=st_nn(Test,Amenities_Clo[Amenities_Clo$feature_type=="veterinary",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearcommercial=st_nn(Test,Amenities_Clo[Amenities_Clo$feature_type=="commercial",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearsalud=st_nn(Test,Amenities_Clo[Amenities_Clo$feature_type=="salud",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearprison=st_nn(Test,Amenities_Clo[Amenities_Clo$feature_type=="prison",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearnightclub=st_nn(Test,Amenities_Clo[Amenities_Clo$feature_type=="nightclub",],returnDist = TRUE, parallel = 7)[["dist"]],
         Neareducacion=st_nn(Test,Amenities_Clo[Amenities_Clo$feature_type=="educacion",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearbus_station=st_nn(Test,Amenities_Clo[Amenities_Clo$feature_type=="bus_station",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearwaste_disposal=st_nn(Test,Amenities_Clo[Amenities_Clo$feature_type=="waste_disposal",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearnightclub=st_nn(Test,Amenities_Clo[Amenities_Clo$feature_type=="nightclub",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearsports=st_nn(Test,Amenities_Clo[Amenities_Clo$feature_type=="sports",],returnDist = TRUE, parallel = 7)[["dist"]],
         NearBusiness=st_nn(Test,Amenities_Clo[Amenities_Clo$feature_type=="Business",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearhotel=st_nn(Test,Amenities_Clo[Amenities_Clo$feature_type=="hotel",],returnDist = TRUE, parallel = 7)[["dist"]],
         NearCBD=st_nn(Test,CBD_Clo,returnDist = TRUE, parallel = 7)[["dist"]],
         Nearpark=st_nn(Test,Amenities_Clo[Amenities_Clo$feature_type=="park",],returnDist = TRUE, parallel = 7)[["dist"]],
         )

Train_Bog<-Train[Train$city=="Bogotá D.C",]%>%
  mutate(NearBank=st_nn(Train[Train$city=="Bogotá D.C",],Amenities_Bog[Amenities_Bog$feature_type=="bank",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearpolice=st_nn(Train[Train$city=="Bogotá D.C",],Amenities_Bog[Amenities_Bog$feature_type=="police",],returnDist = TRUE, parallel = 7)[["dist"]],
         NearComida=st_nn(Train[Train$city=="Bogotá D.C",],Amenities_Bog[Amenities_Bog$feature_type=="Comida",],returnDist = TRUE, parallel = 7)[["dist"]],
         NearES=st_nn(Train[Train$city=="Bogotá D.C",],Amenities_Bog[Amenities_Bog$feature_type=="educacion superior",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearveterinary=st_nn(Train[Train$city=="Bogotá D.C",],Amenities_Bog[Amenities_Bog$feature_type=="veterinary",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearcommercial=st_nn(Train[Train$city=="Bogotá D.C",],Amenities_Bog[Amenities_Bog$feature_type=="commercial",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearsalud=st_nn(Train[Train$city=="Bogotá D.C",],Amenities_Bog[Amenities_Bog$feature_type=="salud",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearprison=st_nn(Train[Train$city=="Bogotá D.C",],Amenities_Bog[Amenities_Bog$feature_type=="prison",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearnightclub=st_nn(Train[Train$city=="Bogotá D.C",],Amenities_Bog[Amenities_Bog$feature_type=="nightclub",],returnDist = TRUE, parallel = 7)[["dist"]],
         Neareducacion=st_nn(Train[Train$city=="Bogotá D.C",],Amenities_Bog[Amenities_Bog$feature_type=="educacion",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearbus_station=st_nn(Train[Train$city=="Bogotá D.C",],Amenities_Bog[Amenities_Bog$feature_type=="bus_station",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearwaste_disposal=st_nn(Train[Train$city=="Bogotá D.C",],Amenities_Bog[Amenities_Bog$feature_type=="waste_disposal",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearnightclub=st_nn(Train[Train$city=="Bogotá D.C",],Amenities_Bog[Amenities_Bog$feature_type=="nightclub",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearsports=st_nn(Train[Train$city=="Bogotá D.C",],Amenities_Bog[Amenities_Bog$feature_type=="sports",],returnDist = TRUE, parallel = 7)[["dist"]],
         NearBusiness=st_nn(Train[Train$city=="Bogotá D.C",],Amenities_Bog[Amenities_Bog$feature_type=="Business",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearhotel=st_nn(Train[Train$city=="Bogotá D.C",],Amenities_Bog[Amenities_Bog$feature_type=="hotel",],returnDist = TRUE, parallel = 7)[["dist"]],
         NearCBD=st_nn(Train[Train$city=="Bogotá D.C",],CBD_Bog,returnDist = TRUE, parallel = 7)[["dist"]],
         Nearpark=st_nn(Train[Train$city=="Bogotá D.C",],Amenities_Bog[Amenities_Bog$feature_type=="park",],returnDist = TRUE, parallel = 7)[["dist"]],
  )


Train_Med<-Train[Train$city=="Medellín",]%>%
  mutate(NearBank=st_nn(Train[Train$city=="Medellín",],Amenities_Med[Amenities_Med$feature_type=="bank",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearpolice=st_nn(Train[Train$city=="Medellín",],Amenities_Med[Amenities_Med$feature_type=="police",],returnDist = TRUE, parallel = 7)[["dist"]],
         NearComida=st_nn(Train[Train$city=="Medellín",],Amenities_Med[Amenities_Med$feature_type=="Comida",],returnDist = TRUE, parallel = 7)[["dist"]],
         NearES=st_nn(Train[Train$city=="Medellín",],Amenities_Med[Amenities_Med$feature_type=="educacion superior",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearveterinary=st_nn(Train[Train$city=="Medellín",],Amenities_Med[Amenities_Med$feature_type=="veterinary",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearcommercial=st_nn(Train[Train$city=="Medellín",],Amenities_Med[Amenities_Med$feature_type=="commercial",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearsalud=st_nn(Train[Train$city=="Medellín",],Amenities_Med[Amenities_Med$feature_type=="salud",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearprison=st_nn(Train[Train$city=="Medellín",],Amenities_Med[Amenities_Med$feature_type=="prison",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearnightclub=st_nn(Train[Train$city=="Medellín",],Amenities_Med[Amenities_Med$feature_type=="nightclub",],returnDist = TRUE, parallel = 7)[["dist"]],
         Neareducacion=st_nn(Train[Train$city=="Medellín",],Amenities_Med[Amenities_Med$feature_type=="educacion",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearbus_station=st_nn(Train[Train$city=="Medellín",],Amenities_Med[Amenities_Med$feature_type=="bus_station",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearwaste_disposal=st_nn(Train[Train$city=="Medellín",],Amenities_Med[Amenities_Med$feature_type=="waste_disposal",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearnightclub=st_nn(Train[Train$city=="Medellín",],Amenities_Med[Amenities_Med$feature_type=="nightclub",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearsports=st_nn(Train[Train$city=="Medellín",],Amenities_Med[Amenities_Med$feature_type=="sports",],returnDist = TRUE, parallel = 7)[["dist"]],
         NearBusiness=st_nn(Train[Train$city=="Medellín",],Amenities_Med[Amenities_Med$feature_type=="Business",],returnDist = TRUE, parallel = 7)[["dist"]],
         Nearhotel=st_nn(Train[Train$city=="Medellín",],Amenities_Med[Amenities_Med$feature_type=="hotel",],returnDist = TRUE, parallel = 7)[["dist"]],
         NearCBD=st_nn(Train[Train$city=="Medellín",],CBD_Med,returnDist = TRUE, parallel = 7)[["dist"]],
         Nearpark=st_nn(Train[Train$city=="Medellín",],Amenities_Med[Amenities_Med$feature_type=="park",],returnDist = TRUE, parallel = 7)[["dist"]],
  )



Train<-rbind(Train_Med,Train_Bog)

saveRDS(Test, file = file.path("stores/Test_Barrios_manzanas_texto_dist.rds"))
saveRDS(Train, file = file.path("stores/Train_Barrios_manzanas_texto_dist.rds"))

################################################################################
Barrios_Bog <- readRDS("stores/Bogotá_Barrios.rds")

Barrios_Bog <- Barrios_Bog %>%                                     
  arrange(desc(Comida))%>%
  select(CODIGO,Comida)%>%
  mutate(city="Bogotá D.C")
Barrios_Bog<-Barrios_Bog[1:5,"CODIGO"]  

Barrios_Bogota<-st_read(file.path("stores/Bogota/barrios_catastrales/barrios_catastrales.shp"))%>%
  st_transform(crs = 4326)
Barrios_Bogota<-Barrios_Bogota%>%
  subset(select=c(NOMB_BARR,BAR_COD,Shape_Leng,Shape_Area,geometry))%>%
  rename(NOMBRE=NOMB_BARR,CODIGO=BAR_COD)

Barrios_Bogota<-left_join(Barrios_Bog,Barrios_Bogota,by="CODIGO")%>%
  st_as_sf(crs=4326) 

##
Barrios_Med <- readRDS("stores/Medellín_Barrios.rds")

Barrios_Med  <- Barrios_Med  %>%                                     
  arrange(desc(Comida))%>%
  select(CODIGO,Comida)%>%
  mutate(city="Medellín")
Barrios_Med<-Barrios_Med[1:5,"CODIGO"]  

Barrios_Medellín<-st_read(file.path("stores/Medellin/planeacion_gdb/planeacion_gdb.shp"))%>%
  st_transform(crs = 4326)
Barrios_Medellín<-Barrios_Medellín%>%
  subset(select=c(CODIGO,NOMBRE,SHAPE__Are,SHAPE__Len,geometry))%>%
  rename(Shape_Area=SHAPE__Are,Shape_Leng=SHAPE__Len)

Barrios_Medellín<-left_join(Barrios_Med,Barrios_Medellín,by="CODIGO")%>%
  st_as_sf(crs=4326) 


##
Barrios_Clo <- readRDS("stores/Cali_Barrios.rds")

Barrios_Clo <- Barrios_Clo %>%                                     
  arrange(desc(Comida))%>%
  select(CODIGO,Comida)%>%
  mutate(city="Cali")
Barrios_Clo<-Barrios_Clo[1:5,"CODIGO"]  

Barrios_Cali<-st_read(file.path("stores/Cali/mc_barrios/mc_barrios.shp"))%>%
  st_transform(crs = 4326)
Barrios_Cali<-Barrios_Cali%>%
  subset(select=c(id_barrio,barrio,shape_leng,shape_area,geometry))%>%
  rename(NOMBRE=barrio,CODIGO=id_barrio,Shape_Area=shape_area,Shape_Leng=shape_leng)


Barrios_Cali<-left_join(Barrios_Clo,Barrios_Cali,by="CODIGO")%>%
  st_as_sf(crs=4326) 


###############

Test<-cbind(Test,as.data.frame(st_distance(Test,Barrios_Cali)))

Train_Bog<-cbind(Train_Bog,as.data.frame(st_distance(Train_Bog,Barrios_Bogota)))

Train_Med<-cbind(Train_Med,as.data.frame(st_distance(Train_Med,Barrios_Medellín)))

Train<-rbind(Train_Med,Train_Bog)

saveRDS(Test, file = file.path("stores/Test_Barrios_manzanas_texto_dist.rds"))
saveRDS(Train, file = file.path("stores/Train_Barrios_manzanas_texto_dist.rds"))


###############
Train_dist <- readRDS("stores/Train_Barrios_manzanas_texto_dist.rds")%>%
  st_as_sf(coords=c("lon","lat"), crs=4326)%>%
  as.data.frame()%>%
  subset(select=c(1,71:92))

Test_dist <- readRDS("stores/Test_Barrios_manzanas_texto_dist.rds")%>%
  st_as_sf(coords=c("lon","lat"), crs=4326)%>%
  as.data.frame()%>%
  subset(select=c(1,71:92))


Train <- readRDS("stores/Train_Barrios_manzanas_texto.rds")%>%
  st_as_sf(coords=c("lon","lat"), crs=4326) 

Test <- readRDS("stores/Test_Barrios_manzanas_texto.rds")%>%
  st_as_sf(coords=c("lon","lat"), crs=4326) 

Train<-left_join(Train,Train_dist,by="property_id")

Test<-left_join(Test,Test_dist,by="property_id")


saveRDS(Test, file = file.path("stores/Test_Barrios_manzanas_texto_dist.rds"))
saveRDS(Train, file = file.path("stores/Train_Barrios_manzanas_texto_dist.rds"))


