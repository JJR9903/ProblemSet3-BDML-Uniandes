

pacman:: p_load(tidyverse, skimr, fastDummies, sf, leaflet, osmdata, tmaptools)
pacman::p_load(parallel,doParallel)

setwd("/Users/JuanJose/Library/CloudStorage/OneDrive-UniversidaddelosAndes/Uniandes/9 Semestre - 1 PEG/Big Data/Problems Set/ProblemSet3-BDML-Uniandes")
stores<-file.path("stores")


set.seed(1234)
n_cores<-detectCores()
cl <- makePSOCKcluster(n_cores - 1) 
registerDoParallel(cl)

#### cargar las viviendas 

Train <- readRDS("stores/Train_TextPorcessed.RDS")%>%
  st_as_sf(coords=c("lon","lat"), crs=4326) 

Test <- readRDS("stores/Test_TextPorcessed.RDS")%>%
  st_as_sf(coords=c("lon","lat"), crs=4326) 

Bogota_properati<-st_as_sf(x = filter(Train,city=="Bogotá D.C"),coords=c("lon","lat"), crs=4326) 
Medellin_properati<-st_as_sf(x = filter(Train,city=="Medellín"),coords=c("lon","lat"), crs=4326) 
Cali_properati<-st_as_sf(x = Test,coords=c("lon","lat"), crs=4326) 

#### cargar los datos de manzanas 
manzana<-st_read(file.path("stores/MGN2021_URB_MANZANA/MGN_URB_MANZANA.shp"))%>%
  st_transform(crs = 4326)
Manzana_Cali<- manzana[manzana$COD_MPIO=="76130",]
Manzana_Bogota<- manzana[manzana$COD_MPIO=="11001",]
Manzana_Medellin<- manzana[manzana$COD_MPIO=="05001",]



#### cargar los dato de barrios 
Barrios_Medellin<-st_read(file.path(stores,"Medellin/planeacion_gdb/planeacion_gdb.shp"))%>%
  st_transform(crs = 4326)

Barrios_Medellin<-Barrios_Medellin%>%
  subset(select=c(OBJECTID,CODIGO,NOMBRE,SHAPE__Are,SHAPE__Len,geometry))%>%
  rename(Shape_Area=SHAPE__Are,Shape_Leng=SHAPE__Len)%>%
  mutate(City="Medellín")

Barrios_Bogota<-st_read(file.path(stores,"Bogota/barrios_catastrales/barrios_catastrales.shp"))%>%
  st_transform(crs = 4326)

Barrios_Bogota<-Barrios_Bogota%>%
  subset(select=c(OBJECTID,NOMB_BARR,BAR_COD,Shape_Leng,Shape_Area,geometry))%>%
  rename(NOMBRE=NOMB_BARR,CODIGO=BAR_COD)%>%
  mutate(City="Bogotá D.C")

Barrios_Train<-rbind(Barrios_Medellin,Barrios_Bogota)

Barrios_Test<-st_read(file.path(stores,"Cali/mc_barrios/mc_barrios.shp"))%>%
  st_transform(crs = 4326)

Barrios_Test<-Barrios_Test%>%
  subset(select=c(id_barrio,barrio,shape_leng,shape_area,geometry))%>%
  rename(NOMBRE=barrio,CODIGO=id_barrio,Shape_Area=shape_area,Shape_Leng=shape_leng)%>%
  mutate(City="Cali")

##### interception de los poligonos de barrios y viviendas 

Test <- st_intersection(x = Test , y = Barrios_Test)
saveRDS(Test, file = file.path("stores/Test_Barrios_texto.rds"))

Train <- st_intersection(x = Train , y = Barrios_Train)
saveRDS(Train, file = file.path("stores/Train_Barrios_texto.rds"))
















##### interception de los poligonos de barrios y viviendas 
Medellin_properati_barrios <- st_intersection(x = Barrios_Train , y = Barrios_Train)
saveRDS(Medellin_properati_barrios, file = file.path("stores/Medellin/barrios_Medellin.rds"))

Bogota_properati_barrios <- st_intersection(x = Bogota_properati , y = Barrios_Bogota)
saveRDS(Bogota_properati_barrios, file = file.path("stores/Bogota/barrios_Bogota.rds"))

##### interception de los poligonos de manzanas y viviendas 

Cali_properati_Manzanas <- st_intersection(x = Cali_properati , y = Manzana_Cali)
Cali<-st_join(x=Cali_properati_barrios,y=Cali_properati_Manzanas, join=st_intersects)
saveRDS(Cali, file = file.path("stores/Cali/Cali.rds"))

Medellin_properati_Manzanas <- st_intersection(x = Medellin_properati , y = Manzana_Medellin)
Medellin<-st_join(x=Medellin_properati_barrios,y=Medellin_properati_Manzanas, join=st_intersects)
saveRDS(Medellin, file = file.path("stores/Medellin/Medellin.rds"))

MM_P_int <- as.list(rep(NA, nrow(Manzana_Medellin)))
for (i in 1:nrow(Manzana_Medellin)){
  print(i)
  MM<-Manzana_Medellin[i,]
  MM_P <- st_intersection(x = Medellin_properati , y = MM)
  MM_P_int[[i]] <- MM_P
}





Bogota_properati_Manzanas <- st_intersection(x = Bogota_properati , y = Manzana_Bogota)
Bogota<-st_join(x=Bogota_properati_barrios,y=Bogota_properati_Manzanas, join=st_intersects)
saveRDS(Bogota_properati_barrios_Manzanas, file = file.path("stores/Bogota/Bogota.rds"))

i = 1
for (id in unique(Manzana_Bogota$id_manzana)){
  print(i,id)
  MB<-filter(Manzana_Bogota,id_manzana==id)
  MB_P <- st_intersection(x = Bogota_properati , y = MB)
  i = i +1 
}

#################################


