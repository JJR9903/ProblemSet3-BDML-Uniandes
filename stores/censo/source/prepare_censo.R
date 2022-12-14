
## load packages
require(pacman) 
p_load(tidyverse,rio)

## censo data
browseURL("https://microdatos.dane.gov.co//catalog/643/get_microdata")

##=== variables ===##

## COD_DANE_ANM: Codigo DANE de manzana
## UA_CLASE: ID
## COD_ENCUESTAS: ID de encuesta
## U_VIVIENDA: ID de vivienda
## H_NRO_CUARTOS: Número de cuartos en total
## HA_TOT_PER: Total personas en el hogar
## V_TOT_HOG: Total de hogares en la vivienda
## VA1_ESTRATO: Estrato de la vivienda (según servicio de energía)

##=== load data ===##

## unzip file
unzip(zipfile="input/11_BOGOTA_CSV.zip" , exdir="input/." , overwrite=T) 

## data manzanas
mgn <- import("input/censo_2018/CNPV2018_MGN_A2_11.CSV")
colnames(mgn)
mgn <- mgn %>% select(COD_DANE_ANM,UA_CLASE,COD_ENCUESTAS,U_VIVIENDA)

## data hogar
hog <- import("input/censo_2018/CNPV2018_2HOG_A2_11.CSV")
colnames(hog)
hog <- hog %>% select(UA_CLASE,COD_ENCUESTAS,U_VIVIENDA,H_NROHOG,H_NRO_CUARTOS,HA_TOT_PER)

## data vivienda
viv <- import("input/censo_2018/CNPV2018_1VIV_A2_11.CSV") 
colnames(viv)
viv <- viv %>% select(COD_ENCUESTAS,UA_CLASE,U_VIVIENDA,V_TOT_HOG,VA1_ESTRATO)

## join hogar-vivienda
viv_hog <- left_join(hog,viv,by=c("COD_ENCUESTAS","U_VIVIENDA","UA_CLASE"))

## joing mnz-hogar-vivienda
viv_hog_mgn <- left_join(viv_hog,mgn,by=c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA"))

##=== collapse data ===##
db <- viv_hog_mgn %>%
      group_by(COD_DANE_ANM) %>% 
      summarise(med_H_NRO_CUARTOS=median(H_NRO_CUARTOS,na.rm=T), 
                sum_HA_TOT_PER=sum(HA_TOT_PER,na.rm=T), 
                med_V_TOT_HOG=median(V_TOT_HOG,na.rm=T),
                med_VA1_ESTRATO=median(VA1_ESTRATO,na.rm=T))

## export data
export(db,"output/mnz_censo_2018.rds")

## delete files
unlink("input/censo_2018/",recursive = T)
unlink("input/__MACOSX/",recursive = T)


