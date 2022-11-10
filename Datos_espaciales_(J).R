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
  #skim(house)

  ####CREACIÓN DE VARIABLES ESPACIALES 

#####Lista de items 
list <-available_tags("leisure") %>% 
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

cway_sev <- osmdata_sp(q1)
sp::plot(cway_sev$osm_points)
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

a2 <- c(osmdata_sf (q4), osmdata_sf (q5)) %>% 
  select(osm_id,amenity) 
a2

z_aprendizajein= a2$osm_points %>% 
  select(osm_id,amenity) 

z_aprendizajein

gc()

####Universidades####


q4 <- opq ("Bogota Colombia")%>%
  add_osm_feature (
    key = "amenity",
    value = "swingerclub")


osm4_sf= q4%>%
  osmdata_sf()
osm4_sf
swinger= osm4_sf$osm_points %>% 
  select(osm_id,amenity) 
swinger


#chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
 #                  featuretype = "boundary:administrative", 
  #                 format_out = "sf_polygon") %>% .$multipolygon
leaflet() %>% addTiles() %>% addPolygons(data=z_uni)
leaflet() %>% addTiles() %>% addCircleMarkers(data=a, col="red")

#parques <- opq(bbox = getbb("UPZ Chapinero, Bogota")) %>%
 # add_osm_feature(key = "amenity", value = "bank") %>%
  #osmdata_sf() %>% .$osm_polygons %>% select(osm_id,name)

#leaflet() %>% addTiles() %>% addPolygons(data=parques)

zonas<-function (city, zone){
  b<-list()
  
  return(b)
}

a<-zonas("Bogotá",c("bank", "university", "restaurant","veterinary", "Pub", "marketplace", "hospital", "clinic", "pharmacy", "prision", "bar", "cafe", "college","nightclub"  ))

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


##Arreglar bases
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
  mutate(amenity="")
osm_al_<-osm_al_%>%
  mutate(amenity="")
osm_al_<-osm_al_%>%
  mutate(amenity="")
osm_al_<-osm_al_%>%
  mutate(amenity="")
osm_al_<-osm_al_%>%
  mutate(amenity="")
osm_al_<-osm_al_%>%
  mutate(amenity="")





parse("b")

b=list()


al <- opq (paste0(city," Colombia"))%>%
  add_osm_feature (
    key = "amenity",
    value = zone[i])
osm_al= al%>%
  osmdata_sf()
parse(paste0("osm_al_",zone[i]))<-osm_al$osm_points%>% 
  select(osm_id,amenity) 
base<-data.frame()
for (i in 1:length(zone)){
  base<-bind_rows(base,parse(paste0("osm_al_",zone[i])))
  }
return(base)

osmdata_sf()

bici <- getbb("Cali Colombia")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("cycleway")) %>%
  osmdata_sf()


ggplot() +
  geom_sf(data = calles$osm_lines,
          inherit.aes = FALSE,
          color = "grey30",
          size = .4,
          alpha = .8) +
  geom_sf(data = bici$osm_lines,
          inherit.aes = FALSE,
          color = "springgreen",
          size = .4,
          alpha = .6) +
  #coord_sf(xlim = c(-0.98, -0.8), 
   #        ylim = c(41.6, 41.7),
    #       expand = FALSE)
#+
  theme_void() +
  labs(title = "cali",
       subtitle = "Carriles bici") +
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(colour = "white"),
        plot.subtitle= element_text(colour = "white"))


ggplot() +
  geom_sf(data = Bogota,
          inherit.aes = FALSE,
          color = "darkgrey",
          size = .3,
          alpha = .8)


+
  coord_sf(xlim = c(-0.98, -0.8), 
           ylim = c(41.6, 41.7),
           expand = FALSE) +
  theme_void() +
  labs(title = "Calí")
