## Eduard Martinez
## update: 27-10-2022

## **[0.] Configuración inicial**

#### **0.1 Instalar/llamar las librerías de la clase**
require(pacman) 
p_load(tidyverse,rio,skimr,viridis,
       gstat, ## variogram
       sf, ## leer/escribir/manipular datos espaciales
       leaflet, ## Visualizaciones dinámicas
       nngeo, ## st_nn function
       spdep, ## Construct neighbours list from polygon list 
       osmdata) ## Get OSM's data

#### **0.2 Importar conjuntos de datos**

## Inmuebles
house <- import("input/house_prices.rds")
class(house)
skim(house)

## dataframe to sf
house <- st_as_sf(x = house, ## datos
                  coords=c("lon","lat"), ## coordenadas
                  crs=4326) ## CRS

st_geometry(house)


## filtrar viviendas en chapinero
chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

house <- house[chapinero,]

leaflet() %>% addTiles() %>% addCircleMarkers(data=house)

## **[1.] Recuperar información de las covariables**

### **1.1 Extraer/modificar patterns en una cadena de caracteres**

## example
word = "Hola mundo, hoy es 19 de julio de 2022"

## Detect Matches
str_detect(string = word , pattern = "19") ## Detect the presence of a pattern match

str_locate(string = word , pattern = "19") ##  Locate the positions of pattern matches in a string

str_count(string = word , pattern = "o") ## Count the number of matches in a string

## Subset Strings
str_extract(string = word , pattern = "19") ## Return the first pattern match found in each strin

str_match(string = word , pattern = "19") ## Return the first pattern match found in each string

str_sub(string = word , start = 1, end = 4)

## Mutate Strings
str_replace(string = word , pattern = "19" , replacement = "10+9")

str_to_lower(string = word)

str_to_upper(string = word)

### **1.2 Expresiones regulares**

## Regular Expressions
str_replace_all(string = word , pattern = " " , replacement = "-")

str_replace_all(string = word , "[:blank:]" , replacement = "-")

str_replace_all(string = word , "19|2022" , replacement = "-")

str_replace_all(string = word , "[0-9]" , replacement = "-")

### **1.3 Aplicación**

## aplicacion
house$description <- tolower(house$description)

house$surface_total[49] ## not surface_total

house$surface_covered[49] ## not surface_covered

house$description[49] ## explore description

x <- "[:space:]+[:digit:]+[:space:]+mts" ## pattern

str_locate_all(string = house$description[49] , pattern = x) ## detect pattern

str_extract(string = house$description[49] , pattern= x) ## extrac pattern

## gen new surfare var
house <- house %>% 
         mutate(new_surface = str_extract(string=description , pattern= x))
table(house$new_surface) %>% sort() %>% head()

### patterns
x1 <- "[:space:]+[:digit:]+[:space:]+"
x2 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+"
house$new_surface <- NA

## replace values
for (i in c("mts","m2","mt2","mts2","metros","cuadrad","mtro","mtr2")){
     house <- house %>% 
              mutate(new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x1,i)),new_surface),
                     new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x2,i)),new_surface))
}

## clean var
for (i in c("mts","m2","mt2","mts2","metros","cuadrad","mtro","mtr2"," ","\n\n")){
     house$new_surface <- gsub(i,"",house$new_surface)
}
house$new_surface <- gsub(",",".",house$new_surface)
house$new_surface <- as.numeric(house$new_surface)

## replace surfare var
table(is.na(house$surface_total))
house$surface_total <- ifelse(is.na(house$surface_total),house$surface_covered,house$surface_total)
table(is.na(house$surface_total))
house$surface_total <- ifelse(is.na(house$surface_total),house$new_surface,house$surface_total)
table(is.na(house$surface_total))

## **[2.] Dependencia espacial**

### **2.1 Motivación**

### **2.2 Semivariograma**

## sf to sp
house_sp <- house %>% as_Spatial()
house_sp

## price
variogram(price/1000000 ~ 1, house_sp, cloud = F , cressie=T) %>% plot()

## variable aleatoria
house_sp$normal <- rnorm(n = nrow(house_sp),
                         mean = mean(house_sp$price/1000000),
                         sd = 1000)

## variogramas
v_price = variogram(price/1000000 ~ 1, house_sp, cloud = F , cressie=T) %>% mutate(estimate=gamma) %>% select(dist,estimate)
v_ramdon = variogram(normal ~ 1, house_sp, cloud = F , cressie=T) %>% mutate(normal=gamma) %>% select(dist,normal)

## join db
db_plot = left_join(x=v_price , y=v_ramdon ,"dist")
db_plot %>% head()

## plot
ggplot(db_plot) + 
geom_point(aes(x=dist, y=normal , fill="Datos aleatorios (Dist. Normal)"), shape=21, alpha=0.5, size=5 ) +
geom_point(aes(x=dist, y=estimate , fill="Precio de la vivienda (properati)"), shape=21, alpha=0.5, size=5 ) +
labs(caption = "Fuente: Properati", y = "Semivariograma", x = "Distancia de separación entre inmuebles", fill = "") + theme_test()

## **[3.] Vecinos espaciales**

### **3.1 MGN**

## cargar manzanas
mnz <- st_read("input/mgn/MGN_URB_MANZANA.shp") %>% select(MANZ_CCNCT) %>% .[chapinero,]
mnz

leaflet() %>% addTiles() %>% 
addPolygons(data=mnz , col="red") 

## unir dos conjuntos de datos basados en la geometría
house <- st_join(x=house , y=mnz)

house %>% select(rooms,bedrooms,bathrooms,surface_total,MANZ_CCNCT)

## Veamos la intuición primero
new_house <- house[st_buffer(house[100,],200),]
new_mnz <- mnz[new_house,]

leaflet() %>% addTiles() %>%
addPolygons(data=new_mnz,col="red") %>%
addCircles(data=new_house)

## unir dos conjuntos de datos basados en la distancia
new_house <- st_join(x=new_house , y=new_mnz , join=st_nn , maxdist=20 , k=1 , progress=F)

new_house %>% select(MANZ_CCNCT.x,MANZ_CCNCT.y)

leaflet() %>% addTiles() %>% 
addPolygons(data=new_mnz , col="red" , label=new_mnz$MANZ_CCNCT) %>% 
addCircles(data=new_house , label=new_house$MANZ_CCNCT.y)

## construir covariables
house <- house %>% group_by(MANZ_CCNCT) %>%
         mutate(surface_mnz=mean(surface_total,na.rm=T)) %>% ungroup()

house %>% select(MANZ_CCNCT,surface_mnz,surface_total)

table(is.na(house$surface_total))
house$surface_total <- ifelse(is.na(house$surface_total),house$surface_mnz,house$surface_total)
table(is.na(house$surface_total))

### **3.2 Censo**

## load data
censo <- import("input/mnz_censo_2018.rds")
censo

## construir covariables
house <- left_join(house,censo,by=c("MANZ_CCNCT"="COD_DANE_ANM"))

table(is.na(house$rooms))
house$rooms <- ifelse(is.na(house$rooms),house$med_H_NRO_CUARTOS,house$rooms)
table(is.na(house$rooms))

### **3.3 Vecinos espaciales**

## obtener objeto sp
new_house_sp <- new_house %>% st_buffer(20) %>% as_Spatial() # poligonos

## obtener vecinos
nb_house = poly2nb(pl=new_house_sp , queen=T) # opcion reina

## vecinos del inmueble 32
nb_house[[32]]

## visualizar
leaflet() %>% addTiles() %>% 
addCircles(data=new_house[nb_house[[32]],]) %>% 
addCircles(data=new_house[32,],col="red") 

## rooms
new_house$rooms[32]

new_house$rooms[nb_house[[32]]]

mean(new_house$rooms[nb_house[[32]]],na.rm=T)


