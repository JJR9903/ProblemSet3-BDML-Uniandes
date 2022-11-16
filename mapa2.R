##limpiar 
rm(list=ls())
require(pacman)
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
##Poner base grande 
Train_Barrios_manzanas_texto_dist <- readRDS("stores/Train_Barrios_manzanas_texto_dist.rds")
Train_Barrios_manzanas_texto_dist <- st_as_sf(x = Train_Barrios_manzanas_texto_dist , ## datos
                                              coords=c("lon","lat"), ## coordenadas
                                              crs=4326) ## CRS

####MEDELLIN####
  ##Leer medellin barrios 
med<-st_read(file.path("~/GitHub/ProblemSet3-BDML-Uniandes/stores/Medellin/planeacion_gdb/planeacion_gdb.shp"))%>%
  st_transform(crs = 4326)

  ##filtro para medallo
Base_med<-Train_Barrios_manzanas_texto_dist %>%
  filter(city=="Medellín")
  
##Hacer medellin barrio comuna
M <- opq(bbox = getbb("Medellin Colombia")) %>%
  add_osm_feature(key="boundary", value="administrative") %>% 
  osmdata_sf()
M <- M$osm_multipolygons %>% subset(admin_level==8)


  ##sacar mean of the prices y mean de restaurantes en una base 
mean_medellin<- Base_med%>%group_by(CODIGO)%>% summarise(bank= sum(bank), comida=sum(Comida), piscina=sum(Piscina), price=mean(price), gym=sum(Gym), park=sum(park) )

  ##pegar los mean a la de comunas 
mapam<- med%>%st_join(mean_medellin)
mapam<- mapam%>%st_join(M)


  ##Sacar los graficos precios
ggplot() +
  geom_sf(data=mapam, aes(fill=price)) +theme_void() +  
  scale_fill_distiller(palette = "RdPu", na.value = "white") +
  labs(title = "Medellín: Distribución del valor de los inmuebles en venta")

##Sacar los graficos comida

ggplot() +
  geom_sf(data=mapam, aes(fill=comida)) +theme_void() +  
  scale_fill_distiller(palette = "RdPu", na.value = "white") +
  labs(title = "Medellín: Distribución del lugares de comida")

ggplot() +
  geom_sf(data=mapam, aes(fill=gym)) +theme_void() +  
  scale_fill_distiller(palette = "RdPu", na.value = "white") +
  labs(title = "Medellín: Distribución de gimnasios")

ggplot() +
  geom_sf(data=mapam, aes(fill=park)) +theme_void() +  
  scale_fill_distiller(palette = "RdPu", na.value = "white") +
  labs(title = "Medellín: Distribución de parques")

ggplot() +
  geom_sf(data=mapam, aes(fill=piscina)) +theme_void() +  
  scale_fill_distiller(palette = "RdPu", na.value = "white") +
  labs(title = "Medellín: Distribución de piscina")

###BOGOTA
bog<-st_read(file.path("stores/Bogota/barrios_catastrales/barrios_catastrales.shp"))%>%
  st_transform(crs = 4326)

Base_bog<-Train_Barrios_manzanas_texto_dist %>%
  filter(city=="Bogotá D.C")

mean_Bogotá<- Base_bog%>%group_by(CODIGO)%>% summarise(bank= sum(bank), comida=sum(Comida), piscina=sum(Piscina), price=mean(price), gym=sum(Gym), park=sum(park) )

mapab<- bog%>%st_join(mean_Bogotá)

##Sacar los graficos precios
ggplot() +
  geom_sf(data=mapab, aes(fill=price)) +theme_void() +  
  scale_fill_distiller(palette = "RdPu", na.value = "white") +
  labs(title = "Bogotá: Distribución del valor de los inmuebles en venta")

ggplot() +
  geom_sf(data=mapab, aes(fill=comida)) +theme_void() +  
  scale_fill_distiller(palette = "RdPu", na.value = "white") +
  labs(title = "Bogotá: Distribución de lugares para comer")

ggplot() +
  geom_sf(data=mapab, aes(fill=gym)) +theme_void() +  
  scale_fill_distiller(palette = "RdPu", na.value = "white") +
  labs(title = "Bogotá: Distribución de gimnasios")

ggplot() +
  geom_sf(data=mapab, aes(fill=park)) +theme_void() +  
  scale_fill_distiller(palette = "RdPu", na.value = "white") +
  labs(title = "Bogotá: Distribución de parques")

ggplot() +
  geom_sf(data=mapab, aes(fill=piscina)) +theme_void() +  
  scale_fill_distiller(palette = "RdPu", na.value = "white") +
  labs(title = "Bogotá: Distribución de piscina")


