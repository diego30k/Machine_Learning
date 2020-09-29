###################################################
##### DMC : Metodos supervisados              #####
##### Metodos h2o - Modelos                   #####
##### Profesor: Daniel Chavez Gallo           #####
###################################################

# Limpiar memoria, eliminar objetos
rm(list = ls())

# Librerias necesarias
library(tidyverse)    # libreria data science, uso de: "%>%", para manipulacion de datos
library(readr)        # libreria que importa data en formato tibble, 5 veces menos pesado que un data.frame
library(tictoc)       # libreria que ayuda a mejorar los tiempos de proceso para transformar una data ---> h2o
library(caret)        # libreria para machine learning, procesos de evalucion y tuneo de parametros
library(ROCR)         # para utilizar curva ROC
library(pROC)         # graficar curva ROC
library(h2o)
library(h2oEnsemble)

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Programar cluster para uso de h2o
h2o.init(max_mem_size = "8g", nthreads = -1)

# Importar la data como rds (extencion propia del R)
datos <- read_csv("data/Datos_mla_train_valid.csv")
dim(datos)
class(datos)
prop.table(table(datos$NUMPERIODO, datos$TARGET),1)

#---------------------------------------------------------------------------------
# Definicion de data de entrenamiento y validacion ------------------------
# semilla para el inicio de la aleatorizacion
set.seed(123)
# Se crean los indices de las observaciones de entrenamiento
id <- createDataPartition(y = datos$TARGET, p = 0.7, list = FALSE, times = 1)
Train <- datos[id, ]
Valid  <- datos[-id, ]
dim(Train)
dim(Valid)

# proporcion de target en el Train
addmargins(table(Train$TARGET))
addmargins(round(100*prop.table(table(Train$TARGET)),6))

# proporcion de target en el Valid
addmargins(table(Valid$TARGET))
addmargins(round(100*prop.table(table(Valid$TARGET)),6))

# cantidad mensual
addmargins(table(Train$NUMPERIODO))
addmargins(table(Valid$NUMPERIODO))

# colnames de variables CRM
col_crm <- colnames(Train %>% select(contains("NUM"))%>% 
                      select(-contains("RENTA")) %>% 
                      select(-contains("INCI")) %>%
                      select(-c("NUMPERIODO"))
                    )

# Data entrenamiento solo con variables de trafico
Train[1:2, 1:5]
datos_trafico <- tibble(Train %>% select(1,2,"TARGET"), Train %>% select(col_crm))
dim(datos_trafico)

# Data Valideo solo con variables de trafico
datos_trafico_V <- tibble(Valid %>% select(1,2,"TARGET"), Valid %>% select(col_crm))
dim(datos_trafico_V)

# transformacion a formato h2o
options("h2o.use.data.table" = TRUE)
tic()
Trafico_h2o_Train_b = as.h2o(datos_trafico)
Trafico_h2o_Valid_b = as.h2o(datos_trafico_V)
toc()

# transformar el traget a factor
Trafico_h2o_Train_b[,3] <- h2o.asfactor(Trafico_h2o_Train_b[,3])
Trafico_h2o_Valid_b[,3] <- h2o.asfactor(Trafico_h2o_Valid_b[,3])
# definir sus variables
yt <- "TARGET" # Target
X1 <- setdiff(colnames(Trafico_h2o_Train_b), yt) # a las etiquetas de variables X, le quitamos el target
X2<- setdiff(X1, "NUMPERIODO")                      # a las etiquetas de variables X, le quitamos el NUMPERIODO
Xt <- setdiff(X2, "CLIENTE")                     # a las etiquetas de variables X, le quitamos el cliente
yt
Xt

# Modelo supervisado random forest
RF_Tra <- h2o.randomForest(x = Xt_filtradas,                                       # Encabezados de variables predictoras
                           y = yt,                                       # Encabezado variable traget
                           model_id ="Trafico_RFo",                      # Nombre del modelo, clave para todos los procesos
                           training_frame = Trafico_h2o_Train_b,         # Base de Train_h2o
                           validation_frame = Trafico_h2o_Valid_b,       # Base de Valid_h2o
                           nfolds = 5,                                   # 5 Validaciones cruzadas
                           seed = 1234,                                  # Semilla, con el cual aseguramos la continuidad
                           fold_assignment = "Modulo",                   # M?todo de validaci?n   
                           keep_cross_validation_predictions = TRUE,     
                           keep_cross_validation_fold_assignment = TRUE,
                           keep_cross_validation_models = TRUE,
                           ntrees = 500,                                 # Arboles involucrados
                           max_depth = 10,                               # Niveles de poda
                           stopping_rounds = 5,                          # Individuos en nodos finales
                           stopping_metric = "AUC",                      # AUC, metrica para target binario
                           stopping_tolerance = 0.001,                   # Tolerancia en ramificaciones 
                           sample_rate = 0.75,                           # Proporcion de columnas seleccionadas al azar
                           binomial_double_trees = TRUE,
                           min_split_improvement = 1e-05                 # Regula el sobreajuste, y minimiza el ECM para que ocurra una division.
                          )

# Variables importantes
#  las variables menos importantes deben quedar de lado
#  las variables repetidas trasnformadas o no, deben seleccionarse por importancia
h2o.varimp(RF_Tra)

# evaluaciones
h2o.confusionMatrix(RF_Tra, valid = TRUE)
h2o.performance(RF_Tra, valid = TRUE)

#--------- funcion
Select_Var <- function(variables){
  a = data.frame(Variables = variables)
  a['index'] = row(a)
  a['extract'] = regmatches(as.character(a$Variables), regexpr("\\w+",as.character(a$Variables)))
  b = a %>% group_by(extract) %>% summarise(minIndex = min(index)) %>% arrange(minIndex)
  varFiltro = as.character(as.data.frame(variables)[b$minIndex,])
  return(varFiltro)
}
#---------
Xt_filtradas <- Select_Var(h2o.varimp(RF_Tra)[1:50,1])

#--------- funcion
indicadores <- function(modelo)
{
  conf_mat <- h2o.confusionMatrix(modelo, valid = TRUE)
  perf_mat <- h2o.performance(modelo, valid = TRUE)
  kpis <- data.frame(Presicion = round(1-conf_mat[3,3],3), 
                     Sensibilidad = round(1-conf_mat[2,3],3), 
                     AUC = round(perf_mat@metrics$AUC,3), 
                     Loglogs = round(perf_mat@metrics$logloss,3), 
                     Gini = round(perf_mat@metrics$Gini,3))
  return(kpis)
}
#---------

indes_Trafico <- indicadores(RF_Tra)
indes_Trafico

# grafica de la importancia de las variables
h2o.varimp_plot(RF_Tra)

# Guardando el modelo
h2o.saveModel(object = RF_Tra, path = "Modelos/", force=TRUE)

# data h2o a trabajar para el modelo, en funcion de las mejores variables de 
# trafico y el resto de CRM
options("h2o.use.data.table" = TRUE)
tic()
Train_H2o = as.h2o(data.frame(Train %>% select(-col_crm), Train %>% select(Xt_filtradas)))
Valid_H2o = as.h2o(data.frame(Valid %>% select(-col_crm), Valid %>% select(Xt_filtradas)))
toc()

h2o.dim(Train_H2o)
h2o.dim(Valid_H2o)
h2o.colnames(Train_H2o)

# Definicion de variables, Xs y Target
y <- "TARGET"
X1 <- setdiff(h2o.colnames(Train_H2o), y)
X2<- setdiff(X1, "NUMPERIODO")
X <- setdiff(X2, "CLIENTE")
X

# nombrar factor a Target
Train_H2o[, y] <- as.factor(Train_H2o[,y])  
Valid_H2o[,y] <- as.factor(Valid_H2o[,y])

# frecuencias
h2o.table(Train_H2o[, "TARGET"])
h2o.table(Valid_H2o[, "TARGET"])

# ropPorciones
prop.table(table(as.data.frame(Train_H2o[, "TARGET"])))
prop.table(table(as.data.frame(Valid_H2o[, "TARGET"])))

# Balanceos con ROSE
library(ROSE)

# over sampling 50-50
over_train = ovun.sample(TARGET ~., data = as.data.frame(Train_H2o), 
                         method = "over", N = 30800, seed =15)$data
over_valid = ovun.sample(TARGET ~., data = as.data.frame(Valid_H2o), 
                         method = "over", N = 13154, seed =15)$data
table(over_train$TARGET); table(over_valid$TARGET)

# under sampling 50-50
under_train = ovun.sample(TARGET ~., data = as.data.frame(Train_H2o), 
                          method = "under", N = 2166, seed =12)$data
under_valid = ovun.sample(TARGET ~., data = as.data.frame(Valid_H2o), 
                         method = "under", N = 972, seed =12)$data
table(under_train$TARGET); table(under_valid$TARGET)

# smote sampling 75-25
smote_train_75 = ovun.sample(TARGET ~., data = as.data.frame(Train_H2o), 
                             method = "both", p = 0.25, seed = 13)$data
smote_valid_75 = ovun.sample(TARGET ~., data = as.data.frame(Valid_H2o), 
                            method = "both", p = 0.25, seed = 13)$data
table(smote_train_75$TARGET); table(smote_valid_75$TARGET)

# smote sampling 80-20
smote_train_80 = ovun.sample(TARGET ~., data = as.data.frame(Train_H2o), 
                             method = "both", p = 0.2, seed = 13)$data
smote_valid_80 = ovun.sample(TARGET ~., data = as.data.frame(Valid_H2o), 
                            method = "both", p = 0.2, seed = 13)$data
table(smote_train_80$TARGET); table(smote_valid_80$TARGET)

# todas las bases train y Valid balanceadas las convertimos a h2o.
options("h2o.use.data.table" = TRUE)
tic()
Train_H2o_over    = as.h2o(over_train)
Train_H2o_under   = as.h2o(under_train)
Train_H2o_smote75 = as.h2o(smote_train_75)
Train_H2o_smote80 = as.h2o(smote_train_80)
toc()

options("h2o.use.data.table" = TRUE)
tic()
Valid_H2o_over    = as.h2o(over_valid)
Valid_H2o_under   = as.h2o(under_valid)
Valid_H2o_smote75 = as.h2o(smote_valid_75)
Valid_H2o_smote80 = as.h2o(smote_valid_80)
toc()

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Proceso modelo over
RF_Over <- h2o.randomForest(x = X_over_filtradas,                                       
                           y = y,                                       
                           model_id ="RF_over",                      
                           training_frame = Train_H2o_over,         
                           validation_frame = Valid_H2o_over,       
                           nfolds = 5,                              
                           seed = 1234,                             
                           fold_assignment = "Modulo",              
                           keep_cross_validation_predictions = TRUE,     
                           keep_cross_validation_fold_assignment = TRUE,
                           keep_cross_validation_models = TRUE,
                           ntrees = 500,                            
                           max_depth = 6,                          
                           stopping_rounds = 50,                     
                           stopping_metric = "AUC",                 
                           stopping_tolerance = 0.001,              
                           sample_rate = 0.75,                      
                           binomial_double_trees = TRUE,
                           min_split_improvement = 1e-05            
                          )

ind_over <- indicadores(RF_Over)
ind_over

h2o.varimp_plot(RF_Over)

X_over_filtradas <- Select_Var(h2o.varimp(RF_Over)[1:25,1])

# Proceso modelo under
RF_Under <- h2o.randomForest(x = X_under_filtradas,                                       
                            y = y,                                       
                            model_id ="RF_under",                      
                            training_frame = Train_H2o_under,         
                            validation_frame = Valid_H2o_under,       
                            nfolds = 5,                              
                            seed = 1234,                             
                            fold_assignment = "Modulo",              
                            keep_cross_validation_predictions = TRUE,     
                            keep_cross_validation_fold_assignment = TRUE,
                            keep_cross_validation_models = TRUE,
                            ntrees = 500,                            
                            max_depth = 6,                          
                            stopping_rounds = 50,                    
                            stopping_metric = "AUC",                 
                            stopping_tolerance = 0.001,              
                            sample_rate = 0.75,                      
                            binomial_double_trees = TRUE,
                            min_split_improvement = 1e-05            
                          )

ind_under <- indicadores(RF_Under)
ind_under

h2o.varimp_plot(RF_Under)

X_under_filtradas <- Select_Var(h2o.varimp(RF_Under)[1:25,1])

# Proceso modelo SMOTE 75 - 25
RF_75 <- h2o.randomForest(x = X_75_filtradas,                                       
                             y = y,                                       
                             model_id ="RF_75",                      
                             training_frame = Train_H2o_smote75,         
                             validation_frame = Valid_H2o_smote75,       
                             nfolds = 5,                              
                             seed = 1234,                             
                             fold_assignment = "Modulo",              
                             keep_cross_validation_predictions = TRUE,     
                             keep_cross_validation_fold_assignment = TRUE,
                             keep_cross_validation_models = TRUE,
                             ntrees = 500,                            
                             max_depth = 6,                          
                             stopping_rounds = 50,                     
                             stopping_metric = "AUC",                 
                             stopping_tolerance = 0.001,              
                             sample_rate = 0.75,                      
                             binomial_double_trees = TRUE,
                             min_split_improvement = 1e-05            
                            )

ind_75 <- indicadores(RF_75)
ind_75

h2o.varimp_plot(RF_75)

X_75_filtradas <- Select_Var(h2o.varimp(RF_75)[1:25,1])

# Proceso modelo SMOTE 80 - 20
RF_80 <- h2o.randomForest(x = X_80_filtradas,                                       
                          y = y,                                       
                          model_id ="RF_80",                      
                          training_frame = Train_H2o_smote80,         
                          validation_frame = Valid_H2o_smote80,       
                          nfolds = 5,                              
                          seed = 1234,                             
                          fold_assignment = "Modulo",              
                          keep_cross_validation_predictions = TRUE,     
                          keep_cross_validation_fold_assignment = TRUE,
                          keep_cross_validation_models = TRUE,
                          ntrees = 500,                            
                          max_depth = 6,                          
                          stopping_rounds = 50,                     
                          stopping_metric = "AUC",                 
                          stopping_tolerance = 0.001,              
                          sample_rate = 0.75,                      
                          binomial_double_trees = TRUE,
                          min_split_improvement = 1e-05            
                         )

ind_80 <- indicadores(RF_80)
ind_80

h2o.varimp_plot(RF_80)

X_80_filtradas <- Select_Var(h2o.varimp(RF_80)[1:25,1])

# Comparacion de KPIs de todos los modelos
KPIS_glm <- rbind(ind_over,ind_under, ind_75, ind_80)
rownames(KPIS_glm) <- c("ind_over","ind_under", "ind_75", "ind_80")
KPIS_glm

# Guardando el modelo
h2o.saveModel(object = RF_75, path = "Modelos/", force=TRUE)

#####################################
##----------Modelo GLM
glm_75 <- h2o.glm(x = X_glm_75_filtradas,
                  y = y,
                  family = "binomial",
                  model_id = "glm_75",                   
                  training_frame = Train_H2o_smote75,
                  validation_frame = Valid_H2o_smote75,
                  nfolds = 5,
                  seed = 1234,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,    
                  keep_cross_validation_fold_assignment = TRUE,
                  keep_cross_validation_models = TRUE
)

ind_75<-indicadores(glm_75)
ind_75

h2o.varimp_plot(glm_75)

X_glm_75_filtradas <- Select_Var(h2o.varimp(glm_75)[1:21,1])

#####################################
##----------Modelo Gradient Boosting Machine
gbm_75 <- h2o.gbm(y = y, 
                   x = X_gbm_75_filtradas, 
                   model_id = "gbm_75", 
                   nfolds = 5,
                   distribution = "bernoulli",
                   training_frame = Train_H2o_smote75, 
                   validation_frame = Valid_H2o_smote75, 
                   seed = 1234,
                   ntrees=500, 
                   learn_rate=0.1, 
                   stopping_rounds = 50,
                   fold_assignment = "Modulo",
                   keep_cross_validation_predictions = TRUE,    
                   keep_cross_validation_fold_assignment = TRUE,
                   keep_cross_validation_models = TRUE
                   )

ind_gbm_75 <- indicadores(gbm_75)
ind_gbm_75

h2o.varimp_plot(gbm_75)

X_gbm_75_filtradas <- Select_Var(h2o.varimp(gbm_75)[1:25,1])

#####################################
##----------Modelo deep learning
deep_75 <- h2o.deeplearning(y = y, 
                            x = X, 
                            model_id = "deep_75", 
                            nfolds = 5,
                            distribution = "bernoulli",
                            stopping_metric = "AUC",
                            training_frame = Train_H2o_smote75, 
                            validation_frame = Valid_H2o_smote75, 
                            seed = 1234,
                            activation = "RectifierWithDropout",
                            fold_assignment = "Modulo",
                            keep_cross_validation_predictions = TRUE,    
                            keep_cross_validation_fold_assignment = TRUE,
                            keep_cross_validation_models = TRUE
                          )

ind_deep_75 <- indicadores(deep_75)
ind_deep_75

h2o.varimp_plot(deep_75)

X_deep_75_filtradas <- Select_Var(h2o.varimp(deep_75)[1:20,1])

#####################################
##----------Modelo Staking
modelos <- c(RF_75, glm_75, deep_75, gbm_75)
m_stack <- h2o.stack(modelos, response_frame = Train_H2o_smote75)
h2o.ensamble_performence(m_stack, Valid_H2o_smote75)

#####################################
##----------Modelo grid search para random forest
grid_75 <- h2o.grid(algorithm = "randomforest",
                    grid_id = "grid_RF",
                    hyper_params = list(
                      ntrees = c(50, 100, 120),
                      max_depth = c(10,7,5),
                      min_rows = c(5,10,20)
                      ),
                    x = X,
                    y = y,
                    training_frame = Train_H2o_smote75, 
                    seed = 1234,
                    nfolds = 5,
                    stopping_metric = "AUC",
                    search_criteria = list(strategy = "Cartesian")
                    )

grid_pe_RF <- h2o.getGrid(grid_id = "grid_RF", 
                         sort_by = "auc", 
                         decreasing = FALSE)

best_model <- h2o.getModel(grid_pe_RF@model_ids[[1]])

# Eliminar elementos y desconectarse de h2o
h2o.removeAll()
h2o.shutdown()
Y
# Limpiar memoria, eliminar objetos
rm(list = ls())












