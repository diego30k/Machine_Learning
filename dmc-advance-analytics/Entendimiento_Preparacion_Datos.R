################################################
##### DMC : Analisis exploratorio de datos #####
##### Preparacion de los datos             #####
##### Profesor: Daniel Chavez Gallo        #####
##### Fecha: 26082020                      #####
################################################
#------------------------------------------------------------------------------------------------#

# Antes, limpiamos el workspace, por si hubiera 
#  algun dataset o informacion cargada
rm(list = ls())

# Cambiar el directorio de trabajo
#  el directorio por defecto sera la 
#  carpeta de donde se abrio el presente scrip
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Vamos a cargar las librerias necesarias, de no ententerlas ir a: 
#   https://www.rdocumentation.org/
#   de no tenerlas instalarlas, hacerlo con la sentencia: 
#   install.packages(c("tidyverse","readr",......), dependencies = T)

library(tidyverse) # dplyr, purr, readr.... %>%
library(readr) # cargar datos en diferentes formatos
library(readxl) # carga data de excel
library(dummies) # ir estudiando

# Vamos a preparar los datos
datos <- read_xlsx("data/Datos_R.xlsx", sheet = "Data")
dim(datos)
str(datos)

# Son variables nominales
datos$FLG_RENO  <- as.character(datos$FLG_RENO)
datos$NUMPERIODO <- as.character(datos$NUMPERIODO)

# Proporcion de la variable target en la base principal
# FLG_RENO: Reenovacion de producto: 
#       1: Renovo, 0: no Renovo
addmargins(table(datos$FLG_RENO))
addmargins(round(100*prop.table(table(datos$FLG_RENO)),1))

# NUMPERIODOs de analisis
datos %>% group_by(NUMPERIODO) %>% summarise(Conteo = n())

# NUMPERIODOs de analisis y target
datos %>% group_by(NUMPERIODO, FLG_RENO) %>% summarise(Conteo = n())
library(sqldf)
sqldf("select NUMPERIODO, FLG_RENO, count(*) Conteo from datos group by NUMPERIODO, FLG_RENO")

# Tabla de frecuencias por target, analisis del experto
PropFLG_RENO <- as.data.frame.matrix(prop.table(table(datos$NUMPERIODO, datos$FLG_RENO),1))
nrow(PropFLG_RENO)

# Colocamos a los names de fila com parte de la tabla de frecuencias por mes
PropFLG_RENO <- data.frame(Mes =rownames(PropFLG_RENO), PropFLG_RENO) %>% arrange(Mes)

# Graficamos la tendencia
PropFLG_RENO %>%
  ggplot(aes(x=Mes, y=X1, group=1)) +
  geom_point() + geom_line() + 
  geom_hline(yintercept=mean(PropFLG_RENO[,3]), color = 'red')+
  ylim(0,0.15)

# Filtrando meses errados por conocimiento del negocio. Control de la variabilidad.
#  Creamos una funci?n "not in"
'%!in%' <- function(x,y)!('%in%'(x,y))

PropFLG_RENO2 <- PropFLG_RENO %>% 
  filter(Mes %!in% c(201803, 201804, 201812))

# Graficamos la tendencia, despues de eliminar meses inconsistentes
PropFLG_RENO2 %>%
  ggplot(aes(x=Mes, y=X1, group=1)) +
  geom_point() + geom_line() + 
  geom_hline(yintercept=mean(PropFLG_RENO2[,3]), color = 'red')+
  ylim(0,0.15)

# Filtramos de la data principal los meses inconsistentes
datos <- datos %>% filter(NUMPERIODO %in% as.character(PropFLG_RENO2[,1]))
table(datos$NUMPERIODO)

# Analisis de medidas de tendencia central y otros.
library(funModeling)
df_status(datos)
# estadisticas univariadas
write.csv(df_status(datos, print_results = FALSE), 
          "data/Resumen_status.csv", row.names = F)
write.csv(cbind(colnames(datos), t(summary(datos))), 
          "data/Resumen_R.csv", row.names = F)

## Ingenieria de variables
#  Transformaci?n, normalizaci?n y outliers
par(mfrow = c(2,4))
summary(datos$U_NUMTRAFICOPAGADO_U3M)
hist(datos$U_NUMTRAFICOPAGADO_U3M, main ="U_NUMTRAFICOPAGADO_U3M")
boxplot(datos$U_NUMTRAFICOPAGADO_U3M, main ="U_NUMTRAFICOPAGADO_U3M")

# transformacion_1, por si hay variables negativas y para evitar el cero en el logaritmo
U_NUMTRAFICOPAGADO_U3M.1 <- log((datos$U_NUMTRAFICOPAGADO_U3M)^2 + 1)
summary(U_NUMTRAFICOPAGADO_U3M.1)
hist(U_NUMTRAFICOPAGADO_U3M.1, main ="U_NUMTRAFICOPAGADO_U3M.1")
boxplot(U_NUMTRAFICOPAGADO_U3M.1, main ="U_NUMTRAFICOPAGADO_U3M.1")

# Estandarizaci?n
U_NUMTRAFICOPAGADO_U3M.E <- scale(datos$U_NUMTRAFICOPAGADO_U3M)
summary(U_NUMTRAFICOPAGADO_U3M.E)
hist(U_NUMTRAFICOPAGADO_U3M.E, main ="U_NUMTRAFICOPAGADO_U3M.E")
boxplot(U_NUMTRAFICOPAGADO_U3M.E, main ="U_NUMTRAFICOPAGADO_U3M.E")

# normalizaci?n
norm01 <- function(X) return((X - min(X))/(max(X) - min(X)))
U_NUMTRAFICOPAGADO_U3M.N <- norm01(datos$U_NUMTRAFICOPAGADO_U3M)
summary(U_NUMTRAFICOPAGADO_U3M.N)
hist(U_NUMTRAFICOPAGADO_U3M.N, main ="U_NUMTRAFICOPAGADO_U3M.N")
boxplot(U_NUMTRAFICOPAGADO_U3M.N, main ="U_NUMTRAFICOPAGADO_U3M.N")

# kurtosis y skewness de U_NUMTRAFICOPAGADO_U3M
library(moments)
resumen_moments = data.frame(
                              Var_Real= c(
                                          kurtosis(datos$U_NUMTRAFICOPAGADO_U3M, na.rm = T),
                                          skewness(datos$U_NUMTRAFICOPAGADO_U3M, na.rm = T),
                                          sd(datos$U_NUMTRAFICOPAGADO_U3M, na.rm = T)
                                          ),
                              Var_log= c(
                                          kurtosis(U_NUMTRAFICOPAGADO_U3M.1, na.rm = T),
                                          skewness(U_NUMTRAFICOPAGADO_U3M.1, na.rm = T),
                                          sd(U_NUMTRAFICOPAGADO_U3M.1, na.rm = T)
                                          ),
                              Var_Estan= c(
                                          kurtosis(U_NUMTRAFICOPAGADO_U3M.E, na.rm = T),
                                          skewness(U_NUMTRAFICOPAGADO_U3M.E, na.rm = T),
                                          sd(U_NUMTRAFICOPAGADO_U3M.E, na.rm = T)
                                          ),
                              Var_Norma= c(
                                          kurtosis(U_NUMTRAFICOPAGADO_U3M.N, na.rm = T),
                                          skewness(U_NUMTRAFICOPAGADO_U3M.N, na.rm = T),
                                          sd(U_NUMTRAFICOPAGADO_U3M.N, na.rm = T)
                                          )
                            )
rownames(resumen_moments) = c("kurtosis","skewness","DesvStan")
resumen_moments

# Imputaci?n
par(mfrow = c(1,3))
histo = hist(U_NUMTRAFICOPAGADO_U3M.1)
box <- boxplot(U_NUMTRAFICOPAGADO_U3M.1)
box$stats
var <- U_NUMTRAFICOPAGADO_U3M.1

# funcion para imputar data, por boxplot
replace_out <- function(var){
  box <- boxplot(var, plot = F)
  lim <- box$stats[c(1,5)]
  for(i in 1:length(var)){
    if(is.na(var[i]) == TRUE)
    {var[i] = mean(sample(runif(1000,lim[1], lim[2]), 100, replace = T))} 
    if(var[i] < lim[1] | var[i] > lim[2])
    {var[i] <- sample(runif(100000,lim[1], lim[2]), 1, replace = T)}
  }
  return(var)
}

new_U_NUMTRAFICOPAGADO_U3M.1 <- replace_out(U_NUMTRAFICOPAGADO_U3M.1)
boxplot(new_U_NUMTRAFICOPAGADO_U3M.1)

# funcion para detectar outliers, por IQR
NA_out <- function(x, na.rm = TRUE, ...) {
  # Quantiles del 25% y 75% de la variable
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm) 
  H_ext <- 1.5 * IQR(x, na.rm = na.rm) 
  # Se crea la variable igual a "x", donde se reemplazara
  #   los extremos hallados por NA
  y_ext <- x
  y_ext[x < (qnt[1] - H_ext)] <- NA
  y_ext[x > (qnt[2] + H_ext)] <- NA
  return(y_ext)
}

# Solo para variables cuantitativas
pos_cha <- which(sapply(datos,class)=="character")
datos_na <- datos %>% select(-pos_cha) %>% map(NA_out)
length(datos_na)

# pasando datos_na a matriz
datos_2 <- matrix(0, nrow(datos %>% select(-pos_cha)), ncol(datos %>% select(-pos_cha)))
for(i in 1:ncol(datos %>% select(-pos_cha))) datos_2[ ,i]  <- datos_na[[i]]
dim(datos_2)
head(datos_2[, 1:50])
colnames(datos_2) <- colnames(datos %>% select(-pos_cha))

# que pasa si eliminamos todos los NAs de las variables ?
datos_omit <- na.omit(datos_2)
dim(datos_2)
dim(datos_omit)

# imputacion, metodo 1
library(lattice)
library(mice)
methods(mice)
datos_2 <- as.data.frame(datos_2)
imputed_data_1 <- mice(datos_2[ ,names(datos_2) %in% c("NUMTRAFICOTOTAL_64M", "EQUIPOS_PRE")], m = 1,
                       maxit = 1, method = "mean", seed = 2018, print = F)
complete.data_1 <- mice::complete(imputed_data_1)
#-
imputed_data_2 <- mice(datos_2[ ,names(datos_2) %in% c("NUMTRAFICOTOTAL_64M", "EQUIPOS_PRE")], m = 1,
                       maxit = 1, method = "pmm", seed = 2018, print=F)
complete.data_2 <- mice::complete(imputed_data_2)
#-
imputed_data_3 <- mice(datos_2[ ,names(datos_2) %in% c("NUMTRAFICOTOTAL_64M", "EQUIPOS_PRE")], 
                       seed=2018, print = F, m = 30)
complete.data_3 <- mice::complete(imputed_data_3)
#-
par(mfrow=c(2,2))
plot(density(datos_2$NUMTRAFICOTOTAL_64M, na.rm = T), col=2, main = "RM-normal")

plot(density(datos_2$NUMTRAFICOTOTAL_64M, na.rm = T), col=2, main = "RM-mean")
lines(density(complete.data_1$NUMTRAFICOTOTAL_64M), col=3)

plot(density(datos_2$EQUIPOS_PRE, na.rm = T), col=2, main = "RM-stocastico")
lines(density(complete.data_2$EQUIPOS_PRE), col=3)

plot(density(datos_2$EQUIPOS_PRE, na.rm = T), col=2, main = "RM-multiple")
lines(density(complete.data_3$EQUIPOS_PRE), col=3)

# Imputacion, metodo 2
library(VIM)
data_knn_imp <- kNN(datos_2[ ,names(datos_2) %in% c("NUMTRAFICOTOTAL_64M", "EQUIPOS_PRE")], k = 5)
summary(data_knn_imp)

par(mfrow=c(1,2))
plot(density(datos_2$NUMTRAFICOTOTAL_64M, na.rm = T), col=2, main = "RM-normal")
lines(density(data_knn_imp$NUMTRAFICOTOTAL_64M), col=3)

plot(density(datos_2$EQUIPOS_PRE, na.rm = T), col=2, main = "RM-normal")
lines(density(data_knn_imp$EQUIPOS_PRE), col=3)

# Imputacion, metodo 3
library(randomForest)
dim(datos_2)
set.seed(111)
datos_3 <- cbind(datos_2, FLG_RENO = datos$FLG_RENO)
datos_numRF <- rfImpute(as.factor(datos_3$FLG_RENO) ~ ., 
                        datos_3[, 5:6], 
                        ntree=3)   
summary(datos_3[, 5:6])
hist(datos_3[, 5], breaks = 10)
summary(datos_numRF)
hist(datos_numRF[, 2], breaks = 10)

# Imputacion, metodo 4
head(datos_2[, 1:6])

for(j in 5:ncol(datos_2)){
  mini <- min(datos_2[, j], na.rm = TRUE)
  maxi <- max(datos_2[, j], na.rm = TRUE)
  for(i in 1:nrow(datos_2)){
    set.seed(i)
    if(mini == maxi) {
      if(is.na(datos_2[ ,j][i]) == TRUE) datos_2[ , j][i] <- mini
    }else {
      if(is.na(datos_2[ ,j][i]) == TRUE){
        datos_2[ , j][i] <- sample(na.omit(datos_2[ , j]), 1, replace = T)
      }
    }
  }
}

imputed_data_3 <- mice.impute.sample(datos_2,len(datos_2))


set.seed(200)
imputed_data_3 <- mice(datos_2, m = 1,maxit = 1, method = "sample", seed = 2018, print = F)
complete_data_3 <- mice::complete(imputed_data_3)
complete_data_3
summary(complete_data_3)
