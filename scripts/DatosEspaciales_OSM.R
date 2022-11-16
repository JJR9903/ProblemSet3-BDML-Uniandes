# Autores: Juan José Rincón , Juan Andres Ospina, Juanita Chacón 
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
pacman::p_load(rstudioapi, tidyverse, sf, rio, osmdata,leaflet, skimr,parallel,doParallel)
pacman::p_load(tidyverse,rio,skimr,viridis,osmdata,
       ggsn, ## scale bar
       raster,stars, ## datos raster
       ggmap, ## get_stamenmap
       sf, ## Leer/escribir/manipular datos espaciales
       leaflet) ## Visualizaciones dinámicas

set.seed(1234)
n_cores<-detectCores()
cl <- makePSOCKcluster(n_cores - 1) 
registerDoParallel(cl)


################################################################################
cities<- c("Bogotá","Medellín","Cali") 
amenities<- c("bank", "police", "fast_food", "university", "restaurant","veterinary", "pub", "marketplace", "hospital", "clinic", "pharmacy", "prison", "bar", "cafe", "college","nightclub","school","library","bus_station","kindergarten","waste_disposal")
leisures<- c("park", "fitness_centre" ,"sports_centre", "fitness_station", "garden", "swimming_pool" ,"swimming_area", "stadium", "dog_park", "sports_centre")
Buildings <- c("commercial", "retail", "office","industrial","hotel")

for (j in 1:length(cities)){
  city=cities[j]
  assign(city,data.frame())
  print(city)
  for (i in 1:length(amenities)){
    print(paste0(city," amenity ",amenities[i]))
    al <- opq (paste0(city," Colombia"))%>%
      add_osm_feature (
        key = "amenity",
        value = amenities[i])%>%
      osmdata_sf()%>%
      .$osm_points%>% 
      dplyr::select(osm_id,geometry)%>%
      mutate(feature=amenities[i],
             city=city)
    assign(city,rbind(get(city),al))
  }  
  
  for (i in 1:length(leisures)){
    print(paste0(city," leisure ",leisures[i]))
    if(leisures[i]=="park"){
      al <- opq (paste0(city," Colombia"))%>%
        add_osm_feature (
          key = "leisure",
          value = leisures[i])%>%
        osmdata_sf()%>%
        .$osm_polygons%>% 
        dplyr::select(osm_id,geometry)%>%
        mutate(feature=leisures[i],
               city=city)
    }else{
      al <- opq (paste0(city," Colombia"))%>%
        add_osm_feature (
          key = "leisure",
          value = leisures[i])%>%
        osmdata_sf()%>%
        .$osm_points%>% 
        dplyr::select(osm_id,geometry)%>%
        mutate(feature=leisures[i],
               city=city)
    }
    assign(city,rbind(get(city),al))
  }  
  
  for (i in 1:length(Buildings)){
    print(paste0(city," building ",Buildings[i]))
    al <- opq (paste0(city," Colombia"))%>%
      add_osm_feature (
        key = "building",
        value = Buildings[i])%>%
      osmdata_sf()%>%
      .$osm_points%>% 
      dplyr::select(osm_id,geometry)%>%
      mutate(feature=Buildings[i],
             city=city)
    assign(city,rbind(get(city),al))
  }  
}

Cali<-Cali%>%
   mutate(feature_type=
            case_when(
              (feature=="bar"|feature=="cafe"|feature=="pub"|feature=="restaurant"|feature=="fast_food")~"Comida",
              (feature=="clinic"|feature=="hospital"|feature=="pharmacy")~"salud",
              (feature=="college"|feature=="university")~"educacion superior",
              (feature=="kindergarten"|feature=="school"|feature=="library")~"educacion",
              (feature=="marketplace"|feature=="commercial"|feature=="retail")~"commercial",
              (feature=="fitness_centre"|feature=="fitness_station"|feature=="sports_centre"|feature=="stadium"|feature=="swimming_area"|feature=="swimming_pool")~"sports",
              (feature=="park"|feature=="garden"|feature=="dog_park")~"park",
              (feature=="office"|feature=="industrial")~"Business"),
          feature_type=ifelse(is.na(feature_type),feature,feature_type)
          )


Bogotá<-Bogotá%>%
  mutate(feature_type=
           case_when(
             (feature=="bar"|feature=="cafe"|feature=="pub"|feature=="restaurant"|feature=="fast_food")~"Comida",
             (feature=="clinic"|feature=="hospital"|feature=="pharmacy")~"salud",
             (feature=="college"|feature=="university")~"educacion superior",
             (feature=="kindergarten"|feature=="school"|feature=="library")~"educacion",
             (feature=="marketplace"|feature=="commercial"|feature=="retail")~"commercial",
             (feature=="fitness_centre"|feature=="fitness_station"|feature=="sports_centre"|feature=="stadium"|feature=="swimming_area"|feature=="swimming_pool")~"sports",
             (feature=="park"|feature=="garden"|feature=="dog_park")~"park",
             (feature=="office"|feature=="industrial")~"Business"),
         feature_type=ifelse(is.na(feature_type),feature,feature_type)
  )


Medellín<-Medellín%>%
  mutate(feature_type=
           case_when(
             (feature=="bar"|feature=="cafe"|feature=="pub"|feature=="restaurant"|feature=="fast_food")~"Comida",
             (feature=="clinic"|feature=="hospital"|feature=="pharmacy")~"salud",
             (feature=="college"|feature=="university")~"educacion superior",
             (feature=="kindergarten"|feature=="school"|feature=="library")~"educacion",
             (feature=="marketplace"|feature=="commercial"|feature=="retail")~"commercial",
             (feature=="fitness_centre"|feature=="fitness_station"|feature=="sports_centre"|feature=="stadium"|feature=="swimming_area"|feature=="swimming_pool")~"sports",
             (feature=="park"|feature=="garden"|feature=="dog_park")~"park",
             (feature=="office"|feature=="industrial")~"Business"),
         feature_type=ifelse(is.na(feature_type),feature,feature_type)
  )


saveRDS(Medellín,"stores/Medellín_osm.rds")
saveRDS(Bogotá,"stores/Bogotá_osm.rds")
saveRDS(Cali,"stores/Cali_osm.rds")


################################################################################
Medellín <- readRDS("stores/Medellín_osm.rds")%>%
  st_transform(crs = 4326)%>%
  subset(select=-c(CODIGO,NOMBRE,Shape_Area,Shape_Leng))

Bogotá <- readRDS("stores/Bogotá_osm.rds")%>%
  st_transform(crs = 4326)%>%
  subset(select=-c(CODIGO,NOMBRE,Shape_Area,Shape_Leng))

Cali <- readRDS("stores/Cali_osm.rds")%>%
  st_transform(crs = 4326)%>%
  subset(select=-c(CODIGO,NOMBRE,Shape_Area,Shape_Leng))


Barrios_Medellin<-st_read(file.path("stores/Medellin/planeacion_gdb/planeacion_gdb.shp"))%>%
  st_transform(crs = 4326)
Barrios_Medellin<-Barrios_Medellin%>%
  subset(select=c(CODIGO,NOMBRE,SHAPE__Are,SHAPE__Len,geometry))%>%
  rename(Shape_Area=SHAPE__Are,Shape_Leng=SHAPE__Len)

Medellín<-st_join(x = Medellín , y = Barrios_Medellin,join=st_nearest_feature )

Barrios_Bogota<-st_read(file.path("stores/Bogota/barrios_catastrales/barrios_catastrales.shp"))%>%
  st_transform(crs = 4326)

Barrios_Bogota<-Barrios_Bogota%>%
  subset(select=c(NOMB_BARR,BAR_COD,Shape_Leng,Shape_Area,geometry))%>%
  rename(NOMBRE=NOMB_BARR,CODIGO=BAR_COD)

Bogotá<-st_join(x = Bogotá , y = Barrios_Bogota,join=st_nearest_feature )


Barrios_Cali<-st_read(file.path("stores/Cali/mc_barrios/mc_barrios.shp"))%>%
  st_transform(crs = 4326)

Barrios_Cali<-Barrios_Cali%>%
  subset(select=c(id_barrio,barrio,shape_leng,shape_area,geometry))%>%
  rename(NOMBRE=barrio,CODIGO=id_barrio,Shape_Area=shape_area,Shape_Leng=shape_leng)

Cali<-st_join(x = Cali , y = Barrios_Cali,join=st_nearest_feature )

saveRDS(Medellín,"stores/Medellín_osm.rds")
saveRDS(Bogotá,"stores/Bogotá_osm.rds")
saveRDS(Cali,"stores/Cali_osm.rds")

################################################################################


Bogotá_Barrios<-Bogotá%>%
  as.data.frame()%>%
  subset(select=-geometry)%>%
  group_by(CODIGO)%>%
  count(feature_type, sort = TRUE)%>%
  pivot_wider(names_from = feature_type, values_from = n)

Bogotá_Barrios<-Bogotá_Barrios%>%replace(is.na(.), 0)


Medellín_Barrios<-Medellín%>%
  as.data.frame()%>%
  subset(select=-geometry)%>%
  group_by(CODIGO)%>%
  count(feature_type, sort = TRUE)%>%
  pivot_wider(names_from = feature_type, values_from = n)

Medellín_Barrios<-Medellín_Barrios%>%replace(is.na(.), 0)


Cali_Barrios<-Cali%>%
  as.data.frame()%>%
  subset(select=-geometry)%>%
  group_by(CODIGO)%>%
  count(feature_type, sort = TRUE)%>%
  pivot_wider(names_from = feature_type, values_from = n)

Cali_Barrios<-Cali_Barrios%>%replace(is.na(.), 0)


saveRDS(Medellín_Barrios,"stores/Medellín_Barrios.rds")
saveRDS(Bogotá_Barrios,"stores/Bogotá_Barrios.rds")
saveRDS(Cali_Barrios,"stores/Cali_Barrios.rds")
