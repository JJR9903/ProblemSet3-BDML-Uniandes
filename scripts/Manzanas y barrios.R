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

pacman:: p_load(tidyverse, skimr, fastDummies, sf, leaflet, osmdata, tmaptools)
pacman::p_load(parallel,doParallel)

set.seed(1234)
n_cores<-detectCores()
cl <- makePSOCKcluster(n_cores - 1) 
registerDoParallel(cl)

#### cargar las viviendas 

Train <- readRDS("stores/Train_TextPorcessed.RDS")%>%
  st_as_sf(coords=c("lon","lat"), crs=4326) 

Test <- readRDS("stores/Test_TextPorcessed.RDS")%>%
  st_as_sf(coords=c("lon","lat"), crs=4326) 

#### #### #### #### #### #### ####  MANZANAS #### #### #### #### #### #### #### #### 

mnz <- st_read("stores/MGN2021_URB_MANZANA/MGN_URB_MANZANA.shp")%>%
  st_transform(crs = 4326)%>%
  subset(select=c(COD_MPIO,geometry,COD_DANE))%>%
  mutate(COD_DANE=as.numeric(COD_DANE))

Manzana_Cali<- mnz[mnz$COD_MPIO=="76001",]
Manzana_Bogota<- mnz[mnz$COD_MPIO=="11001",]
Manzana_Medellin<- mnz[mnz$COD_MPIO=="05001",]

## Cali
CensoM<-read.csv("stores/Censo/ValleDelCauca/CNPV2018_MGN_A2_76.csv")%>%
  subset(select=c(COD_DANE_ANM,UA_CLASE,COD_ENCUESTAS,U_VIVIENDA))
CensoH<-read.csv("stores/Censo/ValleDelCauca/CNPV2018_2HOG_A2_76.csv")%>%
  subset(select=c(H_NRO_CUARTOS,H_NRO_DORMIT,COD_ENCUESTAS,UA_CLASE,U_VIVIENDA))
CensoV<-read.csv("stores/Censo/ValleDelCauca/CNPV2018_1VIV_A2_76.csv")%>%
  subset(select=c(VA1_ESTRATO,COD_ENCUESTAS,UA_CLASE,U_VIVIENDA))
Censo <- left_join(CensoH,CensoV,by=c("COD_ENCUESTAS","U_VIVIENDA","UA_CLASE"))%>%
  left_join(CensoM,by=c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA"))
Censo <- Censo %>%
  group_by(COD_DANE_ANM) %>% 
  summarise(H_NRO_CUARTOS=median(H_NRO_CUARTOS,na.rm=T), med_VA1_ESTRATO=median(VA1_ESTRATO,na.rm=T))

Manzanas_Cali<-left_join(Manzana_Cali,Censo,by=c("COD_DANE"="COD_DANE_ANM"))


## Bogota
CensoM<-read.csv("stores/Censo/Bogota/CNPV2018_MGN_A2_11.csv")%>%
  subset(select=c(COD_DANE_ANM,UA_CLASE,COD_ENCUESTAS,U_VIVIENDA))
CensoH<-read.csv("stores/Censo/Bogota/CNPV2018_2HOG_A2_11.csv")%>%
  subset(select=c(H_NRO_CUARTOS,H_NRO_DORMIT,COD_ENCUESTAS,UA_CLASE,U_VIVIENDA))
CensoV<-read.csv("stores/Censo/Bogota/CNPV2018_1VIV_A2_11.csv")%>%
  subset(select=c(VA1_ESTRATO,COD_ENCUESTAS,UA_CLASE,U_VIVIENDA))
Censo <- left_join(CensoH,CensoV,by=c("COD_ENCUESTAS","U_VIVIENDA","UA_CLASE"))%>%
  left_join(CensoM,by=c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA"))
Censo <- Censo %>%
  group_by(COD_DANE_ANM) %>% 
  summarise(H_NRO_CUARTOS=median(H_NRO_CUARTOS,na.rm=T), med_VA1_ESTRATO=median(VA1_ESTRATO,na.rm=T))

Manzanas_Bogota<-left_join(Manzana_Bogota,Censo,by=c("COD_DANE"="COD_DANE_ANM"))

## Medellin
CensoM<-read.csv("stores/Censo/Antioquia/CNPV2018_MGN_A2_05.csv")%>%
  subset(select=c(COD_DANE_ANM,UA_CLASE,COD_ENCUESTAS,U_VIVIENDA))
CensoV<-read.csv("stores/Censo/Antioquia/CNPV2018_2HOG_A2_05.csv")%>%
  subset(select=c(H_NRO_CUARTOS,H_NRO_DORMIT,COD_ENCUESTAS,UA_CLASE,U_VIVIENDA))
CensoV<-read.csv("stores/Censo/Antioquia/CNPV2018_1VIV_A2_05.csv")%>%
  subset(select=c(VA1_ESTRATO,COD_ENCUESTAS,UA_CLASE,U_VIVIENDA))
Censo <- left_join(CensoH,CensoV,by=c("COD_ENCUESTAS","U_VIVIENDA","UA_CLASE"))%>%
  left_join(CensoM,by=c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA"))
Censo <- Censo %>%
  group_by(COD_DANE_ANM) %>% 
  summarise(H_NRO_CUARTOS=median(H_NRO_CUARTOS,na.rm=T), med_VA1_ESTRATO=median(VA1_ESTRATO,na.rm=T))

Manzanas_Medellin<-left_join(Manzana_Medellin,Censo,by=c("COD_DANE"="COD_DANE_ANM"))

rm(Censo,CensoV,CensoH,CensoM,Manzana_Cali,Manzana_Bogota,Manzana_Medellin)

Manzanas_Train<-rbind(Manzanas_Medellin,Manzanas_Bogota)

Test <- st_join(x = Test, y = Manzanas_Cali,join=st_nearest_feature )

Train <- st_join(x = Train , y = Manzanas_Train,join=st_nearest_feature )


Test <- Test %>% group_by(COD_DANE) %>%
  mutate(surface_mnz=round(mean(surface_total,na.rm=T)),
         rooms_mnz=round(mean(rooms,na.rm=T)),
         bathrooms_mnz=round(mean(bathrooms,na.rm=T))) %>% ungroup()%>%
  mutate(surface_total=ifelse(is.na(surface_total),surface_mnz,surface_total),
         rooms=ifelse(is.na(rooms),rooms_mnz,rooms),
         bathrooms=ifelse(is.na(bathrooms),bathrooms_mnz,bathrooms))

Train <- Train %>% group_by(COD_DANE) %>%
  mutate(surface_mnz=round(mean(surface_total,na.rm=T)),
         rooms_mnz=round(mean(rooms,na.rm=T)),
         bathrooms_mnz=round(mean(bathrooms,na.rm=T))) %>% ungroup()%>%
  mutate(surface_total=ifelse(is.na(surface_total),surface_mnz,surface_total),
         rooms=ifelse(is.na(rooms),rooms_mnz,rooms),
         bathrooms=ifelse(is.na(bathrooms),bathrooms_mnz,bathrooms))



######## #### ####  cargar los datos de barrios  #### #### #### #### #### #### 
Barrios_Medellin<-st_read(file.path("stores/Medellin/planeacion_gdb/planeacion_gdb.shp"))%>%
  st_transform(crs = 4326)

Barrios_Medellin<-Barrios_Medellin%>%
  subset(select=c(CODIGO,NOMBRE,SHAPE__Are,SHAPE__Len,geometry))%>%
  rename(Shape_Area=SHAPE__Are,Shape_Leng=SHAPE__Len)

Barrios_Bogota<-st_read(file.path("stores/Bogota/barrios_catastrales/barrios_catastrales.shp"))%>%
  st_transform(crs = 4326)

Barrios_Bogota<-Barrios_Bogota%>%
  subset(select=c(NOMB_BARR,BAR_COD,Shape_Leng,Shape_Area,geometry))%>%
  rename(NOMBRE=NOMB_BARR,CODIGO=BAR_COD)

Barrios_Train<-rbind(Barrios_Medellin,Barrios_Bogota)

Barrios_Test<-st_read(file.path("stores/Cali/mc_barrios/mc_barrios.shp"))%>%
  st_transform(crs = 4326)

Barrios_Test<-Barrios_Test%>%
  subset(select=c(id_barrio,barrio,shape_leng,shape_area,geometry))%>%
  rename(NOMBRE=barrio,CODIGO=id_barrio,Shape_Area=shape_area,Shape_Leng=shape_leng)

##### interception de los poligonos de barrios y viviendas 

Test <- st_join(x = Test , y = Barrios_Test,join=st_nearest_feature )

Train <- st_join(x = Train , y = Barrios_Train,join=st_nearest_feature )


Medellín_Barrios <- readRDS("stores/Medellín_Barrios.rds")%>%
  as.data.frame()%>%
  mutate(city="Medellín")

Bogotá_Barrios <- readRDS("stores/Bogotá_Barrios.rds")%>%
  as.data.frame()%>%
  mutate(city="Bogotá D.C")

Train_Barrios<-rbind(Medellín_Barrios,Bogotá_Barrios)

Cali_Barrios <- readRDS("stores/Cali_Barrios.rds")%>%
  as.data.frame()

Test<-left_join(Test,Cali_Barrios,by=c("CODIGO"))

Train<-left_join(Train,Train_Barrios,by=c("CODIGO","city"))

Test<-Test%>%
  subset(select=-c(title,description,operation_type))%>%
  replace(is.na(.), 0)
  

Train<-Train%>%
  subset(select=-c(title,description,operation_type))%>%
  replace(is.na(.), 0)


saveRDS(Test, file = file.path("stores/Test_Barrios_manzanas_texto.rds"))
saveRDS(Train, file = file.path("stores/Train_Barrios_manzanas_texto.rds"))







