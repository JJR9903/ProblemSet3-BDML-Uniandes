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

pacman:: p_load(tidyverse,stargazer,caret,tidymodels,glmnet,parallel,doParallel,MLmetrics,themis,rattle,rlang,randomForest,mlr,rpart,rpart.plot,kableExtra,sf, leaflet, osmdata, tmaptools)

set.seed(1234)
n_cores<-detectCores()
cl <- makePSOCKcluster(n_cores - 1) 
registerDoParallel(cl)


Train <- readRDS("stores/Train_Barrios_texto.rds")%>%
  st_as_sf(coords=c("lon","lat"), crs=4326) 

Train<-as.data.frame(Train)%>%
  mutate(obs = case_when(city=="Medellín"~(price-639246711)/(623222205),city=="Bogotá D.C"~(price-869755897)/(899818886)),
         Apartamento= ifelse(property_type=="Apartamento",1,0),
         Medellin= ifelse(city=="Medellín",1,0),
         Bogota= ifelse(city=="Bogotá D.C",1,0))%>%
  subset(select=-c(surface_covered,bedrooms,title,description,operation_type,OBJECTID,CODIGO,NOMBRE,Shape_Area,Shape_Leng,City,geometry,property_type))


##### FUNCION DE PERDIDA #########
Perdida = function(pred,obs){
  delta_y=(obs-pred)/obs
  error=1
  error=case_when(delta_y<(-0.2)~abs(delta_y)*1000,delta_y>=-0.2 & delta_y<=0~(delta_y)^2,delta_y>0 & delta_y<=(0.2/3)~((delta_y)^2+(delta_y)),delta_y>(0.2/3)~abs(delta_y)*1000)
  return(mean(error))
}

CMSE <- function (data, lev = NULL, model = NULL) {
  
  CMSE_Func <- function(pred,obs,na.rm = TRUE){
    delta_y=(obs-pred)/obs
    error=1
    error=case_when(delta_y<(-0.2)~abs(delta_y)*1000,delta_y>=-0.2 & delta_y<=0~(delta_y)^2,delta_y>0 & delta_y<=(0.2/3)~((delta_y)^2+(delta_y)),delta_y>(0.2/3)~abs(delta_y)*1000)
    return(mean(error))
  }
  
  cmse<-CMSE_Func(pred = data$pred ,obs = data$obs)
  
  c(MSE_RELU = cmse)
}

############### MINI TEST DEL TRAIN ########################################################
set.seed(1234)
Mini <- floor(0.1*nrow(Train))
train_ind <- sample(1:nrow(Train), size = Mini)
Mini_Train <- as.data.frame(Train[train_ind, ])

set.seed(1234)
MiniT <- floor(0.7*nrow(Mini_Train))
MiniT_ind <- sample(1:nrow(Mini_Train), size = MiniT)

MiniT_Train<-Mini_Train[MiniT_ind, ]
MiniT_Test<-Mini_Train[-MiniT_ind, ]

MiniT_Train<-MiniT_Train%>%
  mutate(surface_total=ifelse(is.na(surface_total),0,surface_total),
         bathrooms=ifelse(is.na(bathrooms),0,bathrooms),
         PISO=ifelse(is.na(PISO),0,PISO),)%>%
  subset(select=-c(property_id,city,price))

MiniT_Test<-MiniT_Test%>%
  mutate(surface_total=ifelse(is.na(surface_total),0,surface_total),
         bathrooms=ifelse(is.na(bathrooms),0,bathrooms),
         PISO=ifelse(is.na(PISO),0,PISO),)%>%
  subset(select=-c(property_id,city,price))

Train_class_recipe<- recipe(obs ~ ., data = MiniT_Train)

Train_class_recipe<- recipe(obs~surface_total+rooms+bathrooms+AmenitiesHabP+Parqueadero+Remodelado+Cocina+Comedor+Sala+SalaTV+Estudio+BanoSocial+Servicio+Linos+Deposito+Calentador+Lavado+Balcon+Piscina+ZH+Gym+Deportes+Juegos+ZonaSocial+Ascensor+SinAscensor+PVisistantes+Conjunto+Seguridad+PISO+Apartamento,data = MiniT_Train)
############### MODELOS ########################################################



##### RANDOM FOREST #####
set.seed(1234)
tunegrid <- expand.grid(max.depth=c(2,4,6,9), min.node.size = seq(10, 100, length.out = 10))
control = trainControl(method = "cv",number = 5,  allowParallel = TRUE,verboseIter = TRUE,returnData = TRUE,summaryFunction = CMSE)
randomForest<-caret::train(Train_class_recipe,data=MiniT_Train, method='ranger',metric='MSE_RELU',maximize=FALSE,tungeGrid=tunegrid,trControl=control)


##### Ada Boost #####
set.seed(1234)
tunegrid <- expand.grid(nrounds=seq(10, 100, length.out = 10), tree_depth = c(2,4,6,9)  )
control = trainControl(method = "cv",number = 5,  allowParallel = TRUE,verboseIter = TRUE,returnData = FALSE,summaryFunction = CMSE)
AdaBoost<-caret::train(Train_class_recipe,train_C, method='AdaBag',metric='CMSE',tungeGrid=tunegrid,trControl=control,family = "binomial")


##### XGBoost #####
set.seed(1234)
require("xgboost")
xgbGrid = expand.grid(nrounds = c(250,500),max_depth = c(2,3,4,6),eta = c(0.01,0.3,0.5),gamma = c(0,1), min_child_weight = c(10, 25,50),colsample_bytree = c(0.7), subsample = c(0.6))
xgb_trcontrol = trainControl(method = "cv",number = 5,  allowParallel = TRUE,verboseIter = TRUE,returnData = FALSE,summaryFunction = CMSE)
xgb = caret::train(Train_class_recipe,train_C,trControl = xgb_trcontrol,tuneGrid = xgbGrid,method = "xgbTree",metric="CMSE",maximaize= FALSE)

