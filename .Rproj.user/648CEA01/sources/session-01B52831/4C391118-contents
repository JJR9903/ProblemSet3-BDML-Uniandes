


pacman:: p_load(tidyverse, skimr, fastDummies, sf, leaflet, osmdata, tmaptools)
pacman::p_load(parallel,doParallel)
pacman::p_load(rio,glue,
       hexbin,
       patchwork,vip, ## plot: 
       ggrepel, ## plot: geom_text_repel
       stringi,tidytext,stopwords, ## text-data
       tidymodels,finetune,stringr) 


setwd("/Users/JuanJose/Library/CloudStorage/OneDrive-UniversidaddelosAndes/Uniandes/9 Semestre - 1 PEG/Big Data/Problems Set/ProblemSet3-BDML-Uniandes")
stores<-file.path("stores")

#### cargar las viviendas 

Train <- readRDS("stores/dataPS3/train.Rds")
Test <- readRDS("stores/dataPS3/test.Rds")
####### verificar texto ####

# poner todo en mayusculas
Test <- Test %>%
  mutate(description=toupper(description),
         description=stri_trans_general(str = description , id = "Latin-ASCII"))

Text_freq <- Test %>%
  unnest_tokens(output = word, input = description) %>%
  anti_join(get_stopwords("es"),"word")%>% 
  count(word, sort = TRUE)

rep_str = c('UNO'='1','DOS'='2','TRES'='3','CUATRO'='4','CINCO'='5', 'SEIS'='6','SIETE'='7','OCHO'=8,'NUEVE'='9','DIEZ'='10')
Test$description<-str_replace_all(Test$description,rep_str)
rep_str =c("BAO"= "BANO","BAOS"= "BANOS",'HABITACIN'='HABITACION'," HAB "=" HABITACION ",'NIOS'='NIÑOS',"BALCN"="BALCON",' REA '="AREA","PPAL"="PRINCIPAL","HABS"="HABITACIONES","ADMINISTRACIN"="ADMINISTRACION"," ADMIN "="ADMINISTRACION","ADMON"="ADMINISTRACION")
Test$description<-str_replace_all(Test$description,rep_str)
Test$description<-str_replace_all(Test$description,rep_str)
rep_str = c('PRIMER'='1','PRIMERO'='1','SEGUNDO'='2','TERCER'='3','QUINTO'='5', 'SEXTO'='6','SEPTIMO'='7','OCTAVO'=8,'NOVENO'='9','DECIMO'='10','PRIMER'='1','1º'='1','2º'='2','3º'='3','5º'='5', '6º'='6','7º'='7','8º'=8,'9º'='9','10º'='10')
Test$description<-str_replace_all(Test$description,rep_str)
Test$description<-str_replace_all(Test$description,"\\.",". ")

Text_freq <- Test %>%
  unnest_tokens(output = word, input = description) %>%
  anti_join(get_stopwords("es"),"word")%>% 
  count(word, sort = TRUE)

# remplazar los numeros escritos por numeros 
x <- "[:space:]+[:digit:]+[:space:]+BANOS" 
Test$BANOS<- str_extract(string = Test$description , pattern= x)
x <- "[:space:]+[:digit:]+[:space:]+BANO" 
Test$BANOS<- ifelse(is.na(Test$BANOS),str_extract(string = Test$description , pattern= x),Test$BANOS) 
x_n <- "[:digit:]" 
Test$BANOS<- str_extract(string = Test$BANOS , pattern= x_n)%>%
  as.numeric()

Test$bathrooms <- ifelse(is.na(Test$bathrooms),Test$BANOS,Test$bathrooms)



x <- "[:space:]+[:digit:]+[:space:]+CUARTOS" 
Test$CUARTOS<- str_extract(string = Test$description , pattern= x)
x <- "[:space:]+[:digit:]+[:space:]+HABITACIONES" 
Test$CUARTOS<- ifelse(is.na(Test$CUARTOS),str_extract(string = Test$description , pattern= x),Test$CUARTOS) 
x <- "[:space:]+[:digit:]+[:space:]+ESPACIOS" 
Test$CUARTOS<- ifelse(is.na(Test$CUARTOS),str_extract(string = Test$description , pattern= x),Test$CUARTOS) 
x_n <- "[:digit:]" 
Test$CUARTOS<- str_extract(string = Test$CUARTOS , pattern= x_n)%>%
  as.numeric()
Test$CUARTOS<-ifelse(is.na(Test$CUARTOS),0,Test$CUARTOS)

Test$rooms <- ifelse(is.na(Test$rooms),ifelse(Test$CUARTOS>=Test$bedrooms,Test$CUARTOS,Test$bedrooms),Test$rooms)


Test$surface_total <- ifelse(is.na(Test$surface_total),Test$surface_covered,Test$surface_total)

Test$area <- NA
## replace values
for (i in c("MTS","M2","MT2","MTS2","METROS","CUADRADOS","MTRO","MTR2")){
  Test <- Test %>% 
    mutate(area = ifelse(is.na(area)==T,str_extract(string=description , pattern=paste0( "[:space:]+[:digit:]+[:space:]+",i)),area),
           area = ifelse(is.na(area)==T,str_extract(string=description , pattern=paste0("[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+",i)),area))
}

Test$area<- str_extract(string = Test$area , pattern=  "[:digit:]+" )%>%
  as.numeric()

Test$surface_total <- ifelse(is.na(Test$surface_total),Test$area,Test$surface_total)

Test<-Test%>%
  subset(select=-c(BANOS,CUARTOS,area))

#### LISTA DE AMENITIES DE UNA CASA   mil

Test<-Test%>%
  mutate(
    vestier = ifelse(str_detect(Test$description,"VESTIER"),1,0),
    HabPBano = ifelse(str_detect(Test$description,"(?:HABITACION|ALCOBA|CUARTO) PRINCIPAL CON BANO"),1,0),
    HabPBano = ifelse(HabPBano==0,ifelse(str_detect(Test$description,"PRINCIPAL CON BANO"),1,0),0),
    Tina = ifelse(str_detect(Test$description," TINA "),1,0),
    AmenitiesHabP = vestier+HabPBano+Tina
  )%>%
  select(-c(vestier,HabPBano,Tina))


Test<-Test%>%
  mutate(
    Parqueadero = ifelse(str_detect(Test$description,"(?:GARAJE|PARQUEADERO|CARRO|AUTOMOVIL)"),1,0),
    Remodelado = ifelse(str_detect(Test$description,"(?:REMODELA)"),1,0)
  )

Test<-Test%>%
  mutate(
    Cocina = ifelse(str_detect(Test$description,"COCINA"),1,0),
    Comedor = ifelse(str_detect(Test$description,"COMEDOR"),1,0),
    Sala = ifelse(str_detect(Test$description,"(?:SALA|SALON)"),1,0),
    SalaTV = ifelse(str_detect(Test$description,"(?:SALA|SALON) (?:TV|TELEVISION|TELEVISIN)"),1,0),
    SalaTV = ifelse(SalaTV==0,ifelse(str_detect(Test$description,"(?:SALA|SALON) DE (?:TV|TELEVISION|TELEVISIN)"),1,0),0),
    Estudio = ifelse(str_detect(Test$description,"(?:ESTUDIO|OFICINA|BIBLIOTECA)"),1,0),
    BanoSocial = ifelse(str_detect(Test$description,"BANO SOCIAL"),1,0),
    Servicio = ifelse(str_detect(Test$description,"DE SERVICIO"),1,0),
    Servicio = ifelse(Servicio==0,ifelse(str_detect(Test$description,"DEL SERVICIO"),1,0),0),
    Servicio = ifelse(Servicio==0,ifelse(str_detect(Test$description,"(?:CUARTO|CYB|HABITACION|ALCOBA) DE EMPLEADA"),1,0),0),
    Servicio = ifelse(Servicio==0,ifelse(str_detect(Test$description,"OFICIOS"),1,0),0),
    Linos = ifelse(str_detect(Test$description,"LINOS"),1,0),
    Deposito = ifelse(str_detect(Test$description,"DEPOSITO"),1,0),
    Deposito = ifelse(Deposito==0,ifelse(str_detect(Test$description,"CUARTO UTIL"),1,0),0),
    Calentador = ifelse(str_detect(Test$description,"CALENTADOR"),1,0),
    Lavado  = ifelse(str_detect(Test$description,"(?:LAVADERO|LAVANDERIA|LAVADORA)"),1,0),
    Lavado = ifelse(Lavado==0,ifelse(str_detect(Test$description,"(?:PATIO|ZONA) DE ROPAS"),1,0),0),
    Balcon = ifelse(str_detect(Test$description,"(?:BALCON|TERRAZA|PATIO|JARDIN|ANTEJARDIN)"),1,0),
    AmenitiesViviendaSocial = Cocina+Comedor+Sala+Estudio+BanoSocial+Balcon,
    AmenitiesViviendaServicio = Servicio+Lavado+Linos+Deposito+Calentador
  )


#### LISTA DE AMENITIES DEL CONJUNTO

Test<-Test%>%
  mutate(
    Piscina = ifelse(str_detect(Test$description,"(?:PISCINA|PISINA|PICINA)"),1,0),
    ZH = ifelse(str_detect(Test$description,"(?:SAUNA|JACUZZI|TURCO|JACUSI|JACUSSI|JACUCI|ZAUNA|)"),1,0),
    ZH = ifelse(ZH==0,ifelse(str_detect(Test$description,"(?:ZONA|ZONAS) (?:HUMEDA|HUMEDAS)"),1,0),0),
    Gym = ifelse(str_detect(Test$description,"(?:GYM|GIMNASIO)"),1,0),
    Deportes = ifelse(str_detect(Test$description,"(?:TENIS|TENNIS|SQUASH|CANCHA|MULTIPLE|FUTBOL|BASKET|BALONCESTO)"),1,0),
    Juegos = ifelse(str_detect(Test$description,"(?:JUEGO|INFANTIL|GOLFITO)"),1,0),
    ZonaSocial = ifelse(str_detect(Test$description,"(?:ZONA|SALON) (?:SOCIAL|COMUNAL)"),1,0),
    ZonaSocial = ifelse(ZonaSocial==0,ifelse(str_detect(Test$description,"(?:BBQ|ZONAS|JARDIN)"),1,0),0),
    ZonaSocial = ifelse(ZonaSocial==0,ifelse(str_detect(Test$description,"CLUB HOUSE"),1,0),0),
    ZonaSocial = ifelse(ZonaSocial==0,ifelse(str_detect(Test$description,"ZONAS VERDES"),1,0),0),
    Ascensor = ifelse(str_detect(Test$description,"(?:ASCENSOR|ACENSOR|ASENSOR)"),1,0),
    SinAscensor = ifelse(str_detect(Test$description,"SIN (?:ASCENSOR|ACENSOR|ASENSOR)"),1,0),
    Ascensor = ifelse(SinAscensor==1,0,Ascensor),
    PVisistantes = ifelse(str_detect(Test$description,"(?:PARQUEADERO|PARQUEO|GARAJE) (?:DE|PARA|) VISITANTES"),1,0),
    Conjunto = ifelse(str_detect(Test$description,"(?:UNIDAD|CONJUNTO|EDIFICIO|ADMINISTRACION|CONDOMINIO)"),1,0),
    Seguridad = ifelse(str_detect(Test$description,"(?:VIGILANCIA|SEGURIDAD|PORTERIA|PORTERO|CERRADO|CAMARA|CCTV)"),1,0),
    AmenitiesSocialConjunto = Piscina+ZH+Gym+Deportes+Juegos+ZonaSocial,
    AmenitiesOtrosConjunto = Ascensor+Conjunto+Seguridad+PVisitantes
  )

x <- "[:space:]+[:digit:]+[:space:]+PISO" 
Test$PISO<- str_extract(string = Test$description , pattern= x)
Test$PISO<- str_extract(string = Test$PISO , pattern= x_n)%>%
  as.numeric()


############## TRAIN ###########################################################


Train <- Train %>%
  mutate(description=toupper(description),
         description=stri_trans_general(str = description , id = "Latin-ASCII"))

Text_freq <- Train %>%
  unnest_tokens(output = word, input = description) %>%
  anti_join(get_stopwords("es"),"word")%>% 
  count(word, sort = TRUE)

rep_str = c('UNO'='1','DOS'='2','TRES'='3','CUATRO'='4','CINCO'='5', 'SEIS'='6','SIETE'='7','OCHO'=8,'NUEVE'='9','DIEZ'='10')
Train$description<-str_replace_all(Train$description,rep_str)
rep_str =c("BAO"= "BANO","BAOS"= "BANOS",'HABITACIN'='HABITACION'," HAB "=" HABITACION ",'NIOS'='NIÑOS',"BALCN"="BALCON",' REA '="AREA","PPAL"="PRINCIPAL","HABS"="HABITACIONES","ADMINISTRACIN"="ADMINISTRACION"," ADMIN "="ADMINISTRACION","ADMON"="ADMINISTRACION")
Train$description<-str_replace_all(Train$description,rep_str)
Train$description<-str_replace_all(Train$description,rep_str)
rep_str = c('PRIMER'='1','PRIMERO'='1','SEGUNDO'='2','TERCER'='3','QUINTO'='5', 'SEXTO'='6','SEPTIMO'='7','OCTAVO'=8,'NOVENO'='9','DECIMO'='10','PRIMER'='1','1º'='1','2º'='2','3º'='3','5º'='5', '6º'='6','7º'='7','8º'=8,'9º'='9','10º'='10')
Train$description<-str_replace_all(Train$description,rep_str)
Train$description<-str_replace_all(Train$description,"\\.",". ")

Text_freq <- Train %>%
  unnest_tokens(output = word, input = description) %>%
  anti_join(get_stopwords("es"),"word")%>% 
  count(word, sort = TRUE)

# remplazar los numeros escritos por numeros 
x <- "[:space:]+[:digit:]+[:space:]+BANOS" 
Train$BANOS<- str_extract(string = Train$description , pattern= x)
x <- "[:space:]+[:digit:]+[:space:]+BANO" 
Train$BANOS<- ifelse(is.na(Train$BANOS),str_extract(string = Train$description , pattern= x),Train$BANOS) 
x_n <- "[:digit:]" 
Train$BANOS<- str_extract(string = Train$BANOS , pattern= x_n)%>%
  as.numeric()

Train$bathrooms <- ifelse(is.na(Train$bathrooms),Train$BANOS,Train$bathrooms)



x <- "[:space:]+[:digit:]+[:space:]+CUARTOS" 
Train$CUARTOS<- str_extract(string = Train$description , pattern= x)
x <- "[:space:]+[:digit:]+[:space:]+HABITACIONES" 
Train$CUARTOS<- ifelse(is.na(Train$CUARTOS),str_extract(string = Train$description , pattern= x),Train$CUARTOS) 
x <- "[:space:]+[:digit:]+[:space:]+ESPACIOS" 
Train$CUARTOS<- ifelse(is.na(Train$CUARTOS),str_extract(string = Train$description , pattern= x),Train$CUARTOS) 
x_n <- "[:digit:]" 
Train$CUARTOS<- str_extract(string = Train$CUARTOS , pattern= x_n)%>%
  as.numeric()
Train$CUARTOS<-ifelse(is.na(Train$CUARTOS),0,Train$CUARTOS)

Train$rooms <- ifelse(is.na(Train$rooms),ifelse(Train$CUARTOS>=Train$bedrooms,Train$CUARTOS,Train$bedrooms),Train$rooms)


Train$surface_total <- ifelse(is.na(Train$surface_total),Train$surface_covered,Train$surface_total)

Train$area <- NA
## replace values
for (i in c("MTS","M2","MT2","MTS2","METROS","CUADRADOS","MTRO","MTR2")){
  Train <- Train %>% 
    mutate(area = ifelse(is.na(area)==T,str_extract(string=description , pattern=paste0( "[:space:]+[:digit:]+[:space:]+",i)),area),
           area = ifelse(is.na(area)==T,str_extract(string=description , pattern=paste0("[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+",i)),area))
}

Train$area<- str_extract(string = Train$area , pattern=  "[:digit:]+" )%>%
  as.numeric()

Train$surface_total <- ifelse(is.na(Train$surface_total),Train$area,Train$surface_total)

Train<-Train%>%
  subset(select=-c(BANOS,CUARTOS,area))

#### LISTA DE AMENITIES DE UNA CASA

Train<-Train%>%
  mutate(
    vestier = ifelse(str_detect(Train$description,"VESTIER"),1,0),
    HabPBano = ifelse(str_detect(Train$description,"(?:HABITACION|ALCOBA|CUARTO) PRINCIPAL CON BANO"),1,0),
    HabPBano = ifelse(HabPBano==0,ifelse(str_detect(Train$description,"PRINCIPAL CON BANO"),1,0),0),
    Tina = ifelse(str_detect(Train$description," TINA "),1,0),
    AmenitiesHabP = vestier+HabPBano+Tina
  )%>%
  select(-c(vestier,HabPBano,Tina))


Train<-Train%>%
  mutate(
    Parqueadero = ifelse(str_detect(Train$description,"(?:GARAJE|PARQUEADERO|CARRO|AUTOMOVIL)"),1,0),
    Remodelado = ifelse(str_detect(Train$description,"(?:REMODELA)"),1,0)
  )

Train<-Train%>%
  mutate(
    Cocina = ifelse(str_detect(Train$description,"COCINA"),1,0),
    Comedor = ifelse(str_detect(Train$description,"COMEDOR"),1,0),
    Sala = ifelse(str_detect(Train$description,"(?:SALA|SALON)"),1,0),
    SalaTV = ifelse(str_detect(Train$description,"(?:SALA|SALON) (?:TV|TELEVISION|TELEVISIN)"),1,0),
    SalaTV = ifelse(SalaTV==0,ifelse(str_detect(Train$description,"(?:SALA|SALON) DE (?:TV|TELEVISION|TELEVISIN)"),1,0),0),
    Estudio = ifelse(str_detect(Train$description,"(?:ESTUDIO|OFICINA|BIBLIOTECA)"),1,0),
    BanoSocial = ifelse(str_detect(Train$description,"BANO SOCIAL"),1,0),
    Servicio = ifelse(str_detect(Train$description,"DE SERVICIO"),1,0),
    Servicio = ifelse(Servicio==0,ifelse(str_detect(Train$description,"DEL SERVICIO"),1,0),0),
    Servicio = ifelse(Servicio==0,ifelse(str_detect(Train$description,"(?:CUARTO|CYB|HABITACION|ALCOBA) DE EMPLEADA"),1,0),0),
    Servicio = ifelse(Servicio==0,ifelse(str_detect(Train$description,"OFICIOS"),1,0),0),
    Linos = ifelse(str_detect(Train$description,"LINOS"),1,0),
    Deposito = ifelse(str_detect(Train$description,"DEPOSITO"),1,0),
    Deposito = ifelse(Deposito==0,ifelse(str_detect(Train$description,"CUARTO UTIL"),1,0),0),
    Calentador = ifelse(str_detect(Train$description,"CALENTADOR"),1,0),
    Lavado  = ifelse(str_detect(Train$description,"(?:LAVADERO|LAVANDERIA|LAVADORA)"),1,0),
    Lavado = ifelse(Lavado==0,ifelse(str_detect(Train$description,"(?:PATIO|ZONA) DE ROPAS"),1,0),0),
    Balcon = ifelse(str_detect(Train$description,"(?:BALCON|TERRAZA|PATIO|JARDIN|ANTEJARDIN)"),1,0),
    AmenitiesViviendaSocial = Cocina+Comedor+Sala+Estudio+BanoSocial+Balcon,
    AmenitiesViviendaServicio = Servicio+Lavado+Linos+Deposito+Calentador
  )


#### LISTA DE AMENITIES DEL CONJUNTO

Train<-Train%>%
  mutate(
    Piscina = ifelse(str_detect(Train$description,"(?:PISCINA|PISINA|PICINA)"),1,0),
    ZH = ifelse(str_detect(Train$description,"(?:SAUNA|JACUZZI|TURCO|JACUSI|JACUSSI|JACUCI|ZAUNA|)"),1,0),
    ZH = ifelse(ZH==0,ifelse(str_detect(Train$description,"(?:ZONA|ZONAS) (?:HUMEDA|HUMEDAS)"),1,0),0),
    Gym = ifelse(str_detect(Train$description,"(?:GYM|GIMNASIO)"),1,0),
    Deportes = ifelse(str_detect(Train$description,"(?:TENIS|TENNIS|SQUASH|CANCHA|MULTIPLE|FUTBOL|BASKET|BALONCESTO)"),1,0),
    Juegos = ifelse(str_detect(Train$description,"(?:JUEGO|INFANTIL|GOLFITO)"),1,0),
    ZonaSocial = ifelse(str_detect(Test$description,"(?:ZONA|SALON) (?:SOCIAL|COMUNAL)"),1,0),
    ZonaSocial = ifelse(ZonaSocial==0,ifelse(str_detect(Train$description,"(?:BBQ|ZONAS|JARDIN)"),1,0),0),
    ZonaSocial = ifelse(ZonaSocial==0,ifelse(str_detect(Train$description,"CLUB HOUSE"),1,0),0),
    ZonaSocial = ifelse(ZonaSocial==0,ifelse(str_detect(Train$description,"ZONAS VERDES"),1,0),0),
    Ascensor = ifelse(str_detect(Train$description,"(?:ASCENSOR|ACENSOR|ASENSOR)"),1,0),
    SinAscensor = ifelse(str_detect(Train$description,"SIN (?:ASCENSOR|ACENSOR|ASENSOR)"),1,0),
    Ascensor = ifelse(SinAscensor==1,0,Ascensor),
    PVisistantes = ifelse(str_detect(Test$description,"(?:PARQUEADERO|PARQUEO|GARAJE) (?:DE|PARA|) VISITANTES"),1,0),
    Conjunto = ifelse(str_detect(Train$description,"(?:UNIDAD|CONJUNTO|EDIFICIO|ADMINISTRACION|CONDOMINIO)"),1,0),
    Seguridad = ifelse(str_detect(Train$description,"(?:VIGILANCIA|SEGURIDAD|PORTERIA|PORTERO|CERRADO|CAMARA|CCTV)"),1,0),
    AmenitiesSocialConjunto = Piscina+ZH+Gym+Deportes+Juegos+ZonaSocial,
    AmenitiesOtrosConjunto = Ascensor+Conjunto+Seguridad+PVisistantes
  )

x <- "[:space:]+[:digit:]+[:space:]+PISO" 
Train$PISO<- str_extract(string = Train$description , pattern= x)
Train$PISO<- str_extract(string = Train$PISO , pattern= x_n)%>%
  as.numeric()


saveRDS(Train,file.path(stores,"Train_TextPorcessed.RDS"))
saveRDS(Test,file.path(stores,"Test_TextPorcessed.RDS"))
