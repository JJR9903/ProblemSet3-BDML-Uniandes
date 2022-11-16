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
pacman::p_load(rstudioapi, tidyverse, sf, rio, osmdata,leaflet, skimr,parallel,doParallel,class)
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


Train <- readRDS("stores/Train_Barrios_manzanas_texto_dist.rds")%>%
  st_as_sf(coords=c("lon","lat"), crs=4326) 

Test <- readRDS("stores/Test_Barrios_manzanas_texto_dist.rds")%>%
  st_as_sf(coords=c("lon","lat"), crs=4326)

skim<-skim(Train)
skim_Test<-skim(Test)
Train<-Train[complete.cases(prison),]

leaflet() %>% addTiles() %>% addCircles(data=Test[Test$property_id=="7818bfdb2515268f36f561cf",])


Test_ <- st_join(x = Test , y = Barrios_Test, join=st_nearest_feature)

Train <- st_join(x = Train , y = Barrios_Train)
