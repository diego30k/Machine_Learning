##############################################
##### DMC : Metodos no supervisados      #####
##### Profesor: Daniel Chavez Gallo      #####
##### Fecha: 26082020                    #####
##############################################

#-------------------------------------------------------------------------------------------------------------------#

# Limpiamos el workspace
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Instalar librerias necesarias
install.packages(c("ggplot2",
                   "cluster",
                   "fpc",
                   "mclust",
                   "Rtsne",
                   "reshape",
                   "dbscan",
                   "PCAmixdata",
                   "corrplot",
                   "factoextra"
                   ), dependencies = TRUE)

# cargar librerias en el workspace
library("ggplot2")         # graficos
library("cluster")         # clustering
library("fpc")             # indicadores de mejor numero de cluster
library("mclust")          # clustering
library("dbscan")          # clustering
library("PCAmixdata")      # correspondencia multiple
library("corrplot")        # grafico de correlacion
library("Rtsne")           # medoides
library("reshape")         # medoides
library("factoextra")      # dbscan graficos
#-------------------------------------------------------------------------------------------------------------------#

## ANALISIS DE COMPONENTES PRINCIPALES
carros <- read.csv("data/carros.csv", header = TRUE, sep = ",",fileEncoding="latin1")
str(carros)
head(carros)
tail(carros)

# Separando los numericos
numericos <- sapply(carros, is.numeric)
carrosnum <- carros[ , numericos]
carroscualis <- carros[ , !numericos]
head(carrosnum)
head(carroscualis)
dim(carrosnum)

# Componentes
componentestotal <- princomp(carrosnum, cor = TRUE) # si se usa COR = TRUE, se esta estandarizando la data
names(componentestotal)
summary(componentestotal)

# Obtengo el grafico de sedimentacion o scree plot
screeplot(componentestotal)
screeplot(componentestotal, npcs = 13, type = "lines")
biplot(componentestotal)

# Valores guardados dentro de "princomp"
names(componentestotal)
componentestotal$sdev
componentestotal$loadings
componentestotal$center
componentestotal$scale
componentestotal$n.obs
componentestotal$scores
componentestotal$call

componentestotal$loadings[1:13, 1:13]

# Grafico los puntos
ab <- as.data.frame(componentestotal$loadings[1:13, 1:13])

# Tomo los valores absolutos para ver la importancia
ab$abspc1 <- ab$Comp.1 # abs()
ab$abspc2 <- ab$Comp.2 # abs()
ggplot(ab, aes(x = abspc1, y = abspc2)) + 
  geom_point() + 
  geom_text(aes(label = rownames(ab)))

# efecto de la correlacion
corrplot(cor(carrosnum), method = "number")

# Guardo los componentes y los uno a la base principal
data_new <- componentestotal$scores[,c(1,2)]
data_new
corrplot(cor(data_new), method = "number")

#-------------------------------------------------------------------------------------------------------------------#
## ANALISIS DE CORRESPONDENCIA MULTIPLE
str(carros)
X.quanti <- splitmix(carros)$X.quanti
X.quali <- splitmix(carros)$X.quali
dim(X.quanti)
dim(X.quali)
pca <- PCAmix(X.quanti, X.quali)
pca <- PCAmix(X.quanti, X.quali, graph = TRUE)
pca$eig
pca$ind$coord
corrplot(cor(pca$ind$coord), method = "number")

# Verificacion
library(corrplot)
Mat_cor <- cor(carrosnum)
corrplot(Mat_cor, method = "number")
corrplot(cor(data_new), method = "number")

# Asociaci?n para solo cualitativas
table(carros$tipo, carros$hecho_o_no_en_USA)
chisq.test(carros$tipo, carros$hecho_o_no_en_USA)
#p-value = 0.007112 < 0.05, por lo tanto se rechaza la Ho y aceptamos
                          #que el tipo de carro y donde fue fabricado
                          #son dependientes o estan asociadas

#-------------------------------------------------------------------------------------------------------------------#
## ANALISIS DE CLUSTER JERARQUICO
# Estandarizando
carros_nor_z <- scale(carrosnum)
summary(carrosnum)
summary(carros_nor_z)

# Comparacion grafica
par(mfrow = c(1,2))
hist(carrosnum$caballos_fuerza)
hist(carros_nor_z[,4], main = "Z")

# Obtengo una matriz de correlaciones de todos contra todos
par(mfrow = c(1,1))
matrizcor <- cor(carrosnum)
corrplot(matrizcor, method = "ellipse")

# Metricas de distancia: usar dist()
ejemploeuclid <- dist(carros_nor_z[2:15, ], method = "euclidean")
carrosdist <- dist(carros_nor_z, method = "euclidean")
dim(carros_nor_z)
length(carrosdist)

# Para jerarquico aglomerativo hclust() o agnes() de cluster, diana()
carrosjerarq <- hclust(carrosdist, method = "ward.D")
names(carrosjerarq)
carrosjerarq$merge

# mostarlo con labels
plot(carrosjerarq, labels = carrosjerarq$label)

# grupos
rect.hclust(carrosjerarq, k = 3, border = "blue")

# Altura
rect.hclust(carrosjerarq, h = 20, border = 3:4)

# Cortar el dendograma a un nivel dado y guardar el grupo de pertenencia
carrosjcluster <- cutree(carrosjerarq, k = 3)

# Unir a la base de datos para analisis subsiguiente
carros_grupo_je <- cbind(carros, carrosjcluster)
table(carros_grupo_je$carrosjcluster)
View(carros_grupo_je)
#-------------------------------------------------------------------------------------------------------------------#

## ANALISIS DE CLUSTER KMEANS
summary(carros_nor_z)
wss <- (nrow(carros_nor_z)-1)*sum(apply(carros_nor_z, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(carros_nor_z, centers = i)$withinss) #la varianza de cada grupo
plot(2:15, wss[-1], type="b", xlab="Numero de grupos", ylab="Dentro de los grupos, SS")

# Verificamnos la significancia del numero de cluster propuesto por la silueta
#   si cada indicador es mayor al 70%, el numero de grupos es idoneo
#   si cada indicador es mayor al 85%, existe sobreajuste
library(fpc)
clusterboot(carros_nor_z, B = 100, clustermethod = kmeansCBI, k = 3, seed=5)$bootmean

# Creando los grupos, resultado de la validacion previa
set.seed(25)
data_kmean <- kmeans(carros_nor_z, 3, nstart = 50)
data_kmean$centers

#---------------------------------------------------------------------------------------------------------------#
## ANALISIS DE CLUSTER PAM
# Vamos a calcular la distancia de Gower para los datos que tenemos
#nominales <- which(sapply(carros,class)=="character")

gower_dist <- daisy(carrosnum, metric = "gower",
                    type = list(logratio= 8))
gower_dist

# Una buena manera de entender lo que hace la distancia de Gower es sacar las parejas mas "parecidas"
#   y las menos "parecidas".
gower_mat <- as.matrix(gower_dist)

# La pareja mas similar
carros[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# La pareja menos "similar"
carros[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Ahora vamos a calcular el "numero de grupos" de clientes
set.seed(123)
sil_width <- c(0)
for(i in 2:10){
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

plot(2:10, sil_width[-1],
     xlab = "Numero de grupos",
     ylab = "Anchura de la silueta")
lines(2:10, sil_width[-1])

# Aquella que minimice el tamano de la silueta
clusterboot(gower_dist, B = 100, clustermethod = pamkCBI, k = 3, seed = 123)$bootmean

set.seed(123)
pam_fit <- pam(gower_dist, diss = TRUE, k = 3)

library(dplyr)
pam_results <- carros %>%
                dplyr::select() %>%
                mutate(cluster = pam_fit$clustering) %>%
                group_by(cluster) %>%
                summarise(N = n())
pam_results

# Sacamos los medoides
carros[pam_fit$medoids, ]
#-------------------------------------------------------------------------------------------------------------------#
## ANALISIS DE CLUSTER CLARA

# Aquella que minimice el tamano de la silueta
clusterboot(carros_nor_z, B = 100, clustermethod = claraCBI, k = 3, seed = 123)$bootmean

# Aplicando clara
rownames(carrosnum) <- 1:nrow(carros)
clara_clust <- clara(carrosnum, 3, samples=5, stand = TRUE)
clara_clust
clara_clust$clusinfo
clara_clust$medoids

# Graficos
plot(clara_clust)

fviz_cluster(object = clara_clust, ellipse.type = "t", geom = "point",
             pointsize = 2.5) +
  theme_bw() +
  labs(title = "Resultados clustering CLARA") +
  theme(legend.position = "none")

carros_clara <- data.frame(carrosnum, cluster = clara_clust$cluster)

#-------------------------------------------------------------------------------------------------------------------#
## ANALISIS DE CLUSTER DBSCAN
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")

# Abrir la data de prueba
data("multishapes")
df <- multishapes[, 1:2]
plot(df)

# Definimos una semilla
set.seed(123)
km.res <- kmeans(df, 5, nstart = 25)
fviz_cluster(km.res, df, frame = FALSE, geom = "point")

# Aplicando algoritmo
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)
plot(db, df, main = "DBSCAN", frame = FALSE)

# Graficando clusters
fviz_cluster(db, df, stand = FALSE, frame = FALSE, geom = "point")

#resultado
print(db)

#grupos
db$cluster[sample(1:1089, 50)]

# graficos de sedimentacion
dbscan::kNNdistplot(df, k =  5)
abline(h = 0.15, lty = 2)
abline(v = 5, lty = 2)

#-------------------------------------------------------------------------------------------------------------------#
######################################## Sistemas de recomendacion

##### Basado en USERs
library(tidyverse)
# Para aplicar sistemas de recomendacion basados en filtrado colaborativo 
# es necesario definir una serie de elementos.
#   * Set de usuarios U=u1,u2,...,um --- * Set de items I=i1,i2,...,in
#   * Matriz de valoracion R de dimensiones m x n, donde cada fila representa 
#     a un usuario y cada columna un item. El valor Rjk es la valoracion del 
#     usuario uj sobre el item ik. R puede ser una matriz dispersa, no todos
#     los usuarios tienen que haber valorado todos los items.
#   * Usuario sobre el que se quiere hacer una prediccion (usuario activo) ux.

### Ejemplo:
# Supongase que se dispone del historial de valoraciones que 4 usuarios (u1,u2,...,u4)
# han hecho sobre 5 items (i1,i2,...,i5). Un nuevo usuario (ux) no ha valorado 
# el item (i5). Se pretende aplicar un sistema de recomendacion colaborativo 
# para predecir la valoracion del usuario (ux) sobre el item i5.

# Creamos los datos que vamos a usar
datos <- matrix(c(5, 3, 4, 4, NA, 3, 1, 2, 3, 3, 4, 3, 4, 3, 5, 3, 3, 1, 5, 4, 1,
                  5, 5, 2, 1),  nrow = 5, byrow = TRUE)
colnames(datos) <- c("i_1", "i_2", "i_3", "i_4", "i_5")
rownames(datos) <- c("u_x", "u_1", "u_2", "u_3", "u_4")
datos

# Los sistemas de filtrado colaborativo basado en usuarios predicen la valoracion
#   que un determinado usuario hara sobre un producto utilizando las valoraciones
#   que han hecho sobre ese mismo producto los n usuarios mas parecidos a él. 
#   La similitud entre usuarios se mide acorde al patron de valoraciones que tiene
#   cada uno, en este caso, las filas de la matriz.

# En primer lugar, se calcula la similitud entre el usuario ux y el resto de usuarios. 
#   En este caso, se emplea como medida de similitud el coeficiente de correlacion
#   de Pearson. Como los usuarios estan definidos por las filas, hay que transponer
#   la matriz para realizar los calculos.
matriz_dist <- datos %>% t() %>% cor(method = "pearson", use = "complete.obs")
matriz_dist %>%  as.data.frame %>% rownames_to_column(var = "Usuario_A") %>%
  gather(key = "Usuario_B", value = "corr", -Usuario_A) %>%
  filter(corr != 1 & Usuario_A == "u_x") %>%
  arrange(desc(corr))

# Una vez ordenados los usuarios de mayor a menor similitud respecto al usuario ux, 
#   se procede a calcular la prediccion de la valoracion. Existen varias formas de hacerlo:

# (a) Promedio las valoraciones (Rjk) de los "n" usuarios mas cercanos. Con esto, se evita
#     tener en cuenta la valoracion de usuarios que tienen un perfil muy distinto del 
#     usuario de interes. "n" se debe considerar como un hiperparametro cuyo valor 
#     optimo se identifica, por ejemplo, mediante validacion cruzada. Vease el resultado
#     de este problema si se emplea n = 3. 
#   Los 3 usuarios mas similares a ux son : u1, u2, u3. La prediccion de la valoracion
#   que hace el usuario ux sobre el item i5 se obtiene como la media de las valoraciones
#   que cada uno de los usuarios seleccionados tiene sobre el item 5.
prediccion <- mean(3, 5, 4)
prediccion #la valoracion del usuario ux para el item 5 es igual a = 3

# (b) El inconveniente de la aproximacion anterior es que los n usuarios seleccionados
#     tienen el mismo peso en la prediccion, sin embargo, no todos se parecen en la misma
#     medida al usuario de interes. Una forma de compensar esta influencia es ponderando 
#     la media con los valores de similitud, de esta forma, la valoracion de los usuarios
#     pesa mas cuanto mas se parecen al usuario estudiado. Esta estrategia solo puede
#     aplicarse cuando la similitud toma valores en el rango [0, numero positivo], ya que, 
#     la media aritm?tica ponderada, no esta definida para pesos negativos y, al menos uno
#     de los pesos, debe ser mayor de cero. Otra opcion es considerar las similitudes 
#     negativas como 0 de forma que no contribuyen en el calculo.
prediccion <- (0.85 * 3 + 0.71 * 5 + 0 * 4) / (0.85 + 0.71 + 0)
prediccion #la valoracion del usuario ux para el item 5 es igual a = 3.9

# Los items que tienen siempre valoraciones muy positivas aportan poca informacion
#   sobre el perfil de usuarios

##### Basado en ITEM 
# Creamos los datos que vamos a necesitar
datos <- matrix(c(5, 3, 4, 4, NA, 3, 1, 2, 3, 3, 4, 3, 4, 3, 5, 3, 3, 1, 5, 4, 1,
                  5, 5, 2, 1),  nrow = 5, byrow = TRUE)
colnames(datos) <- c("i_1", "i_2", "i_3", "i_4", "i_5")
rownames(datos) <- c("u_x", "u_1", "u_2", "u_3", "u_4")
datos

# La idea es muy similar al metodo basado en usuarios, pero en este caso, se identifican
#   items similares (empleando el perfil de valoraciones que han recibido) en lugar de 
#   usuarios similares. Ademas, los items que participan en el proceso tienen que haber
#   sido valorados por el usuario de interes ux.
# 
# En primer lugar, se calcula la similitud entre el item i5 y el resto de items. 
#   En la matriz datos, se corresponde con la similitud entre columnas. En este ejemplo,
#   se emplea como medida de similitud el coeficiente de correlacion de Pearson.
matriz_dist <- datos %>% cor(method = "pearson", use = "complete.obs")
matriz_dist %>% as.data.frame %>% rownames_to_column(var = "Item_A") %>%
  gather(key = "Item_B", value = "corr", -Item_A) %>%
  filter(corr != 1 & Item_A == "i_5") %>%
  arrange(desc(corr))

# Una vez calculadas las similitudes entre el item i5 y el resto, se seleccionan 
#   los n items mas parecidos y se obtiene la prediccion a partir de las valoraciones
#   que el usuario ux ha hecho de esos n items.
# (a) Prediccion basada en el promedio de los n = 3 items mas parecidos (i1, i4, i3)
prediccion <- mean(c(5, 4, 4))
prediccion
4.33

# (b) Prediccion basada en el promedio ponderado por similitud:
# Los valores de correlacion negativos se sustituyen por 0 para poder calcular
# una media ponderada.
prediccion <- (5*0.9694584 + 4*0.5816751 - 4*0) / (0.9694584 + 0.5816751 - 0)
prediccion

##### Recomendar peliculas
library(recommenderlab)
# El set de datos MovieLense del paquete recommenderlab, contiene informacion sobre mas de 1000 peliculas, 
#   tanto variables descriptivas de cada largometraje como las valoraciones de mas de 900 usuarios. Empleando 
#   este set de datos, se generan 3 tipos de sistemas de recomendacion con el objetivo de recomendar 10 
#   nuevas peliculas al usuario 329.

# Se ha elegido el usuario 329 porque el numero de peliculas que ha visto se corresponde a la mediana de 
#   peliculas vistas por los usuarios. 

# Cargamos los datos que vamos a necesitar
data("MovieLense")
str(MovieLense)

# No es posible pasar de realRatingMatrix a dataframe directamente.
# Se extraen las valoraciones de las peliculas y se almacenan en formato matriz.
# Cada fila de la matriz contiene la informacion de un usuario y cada columna la 
#   informacion de una pelicula.
valoraciones <- as(MovieLense, "matrix")

# Se convierte la matriz a dataframe
valoraciones <- as.data.frame(valoraciones)
valoraciones <- valoraciones %>% rownames_to_column(var = "usuario")

# Datos descriptivos de las peliculas
atributos <- MovieLenseMeta

# Se reestructuran los datos para que tengan un formato tidy
valoraciones_tidy <- valoraciones %>% gather(key = "pelicula",
                                             value = "valoracion",
                                             -usuario)
head(valoraciones_tidy %>% arrange(usuario), 100)
dim(valoraciones_tidy)

# exportar
write.csv(valoraciones_tidy %>% filter(!is.na(valoracion))%>%
            group_by(usuario) %>%
            summarise(Num_peliculas = n())
          , "peli_usuer.csv", row.names = F)


# Vamos a visualizar la densidad
valoraciones_tidy %>% filter(!is.na(valoracion)) %>%
  ggplot(aes(x = usuario, y = pelicula, fill = valoracion)) +
  geom_tile(color = "black") +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")

# Se cuenta el numero de NA por columna del dataframe y con la funciun reduce
# se suman todos los resultados. La columna usuario se excluye del contaje.
total_NA <- valoraciones %>% select(-usuario) %>%
  map_dbl(.f = function(x){ sum(is.na(x))}) %>%
  reduce(.f = sum)

total_elementos <- (ncol(valoraciones) - 1) * (nrow(valoraciones))
porcentaje_NA   <- 100 * (total_NA / total_elementos)
porcentaje_NA

# Numero de valoraciones por usuario 
valoraciones_tidy %>% filter(!is.na(valoracion)) %>%
  group_by(usuario) %>% count() %>%
  pull(n) %>% median()

# El set de datos contiene las valoraciones de 943 usuarios sobre un total de 1664 peliculas. Sin embargo, 
#   hay que tener en cuenta que se trata de una matriz incompleta (94% de valores ausentes), cada pelicula ha 
#   sido valorada inicamente por una pequeña fraccion de los usuarios. La mediana de valoraciones por usuario
#   es de 64 peliculas. 

# Distribucion de las valoraciones 
valoraciones_tidy %>% 
  filter(!is.na(valoracion)) %>% 
  select(valoracion) %>% 
  group_by(valoracion) %>% 
  count() %>%
  ggplot(aes(x = valoracion, y = n)) +
  geom_col(color = "black") +
  theme_bw()

# Media y mediana de las valoraciones
valoraciones_tidy %>% pull(valoracion) %>% median(na.rm = TRUE)
valoraciones_tidy %>% pull(valoracion) %>% mean(na.rm = TRUE)

# El valor medio y mediana de las valoraciones muestra que los usuarios tienden a valorar 
#    positivamente las peliculas (la media esperada de una distribuci?n uniforme de 1 a 5 es 3). 
# Estandarizacion de las valoraciones por usuario 
valoraciones_tidy <- valoraciones_tidy %>%
  group_by(usuario) %>% 
  mutate(valoracion = scale(valoracion)) %>% # (val - media(val)) sd(val)
  ungroup()

# Atributos de las peliculas
glimpse(atributos)

# Entre los atributos descriptivos de cada pelicula se encuentran el titulo, 
# el anio, una direccion web y 19 posibles tematicas.
atributos %>% select(year) %>% group_by(year) %>% count() %>% 
  ggplot(aes(x =  as.factor(year), y = n)) +
  geom_col() +
  theme_bw() +
  labs(x = "year") +
  theme(axis.text.x = element_text(angle = 90, size = 6))

atributos %>% select(-title, -url, -year) %>%
  gather(key = "variable", value = "valor") %>%
  filter(valor != 0) %>% group_by(variable) %>%
  count() %>% 
  ggplot(aes(x =  reorder(variable, desc(n)), y = n)) +
  geom_col() +
  theme_bw() +
  labs(x = "tem?tica") +
  theme(axis.text.x = element_text(angle = 90))

# Un analisis sencillo de los datos muestra que la mayoria de las peliculas disponibles en el set 
#   de datos son de 1990 o posteriores, y que las tematicas mas frecuentes son drama y comedia. 
#   Para los siguientes analisis, las variables url y year no se emplean, por lo que se excluyen.
atributos <- atributos %>% select(-url, -year)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   (1) Sistema de recomendacion basado en contenido

#   La estrategia seguida para recomendar 10 peliculas al usuario 329 mediante un 
#   sistema basado en contenido es la siguiente:
#   (1.1)Identificar todas las peliculas que el usuario no ha visto. Se asume que 
#        son aquellas para las que el usuario no ha dado su valoracion.
#   (1.2) Para cada una de las p peliculas seleccionadas en el paso 1:
#       * Calcular su similitud con las peliculas vistas por el usuario. En este caso, 
#         dado que los atributos son binarios, se emplea como medida de similitud el indice de Jaccard.
#       * Seleccionar las n=15 peliculas m?s parecidas. En la pr?ctica, el n?mero ?ptimo de peliculas deber?a identificarse 
#           mediante validaci?n cruzada, sin embargo, para no a?adir una capa de complejidad extra al ejemplo, se emplea este valor.
#       * Se calcula la media ponderada de las valoraciones que el usuario 329 ha dado de las n=15 peliculas m?s parecidas. 
#           Este valor se almacena como el valor predicho para la pelicula p.
#   (1.3) Se muestran como recomendaciones las 10 peliculas con mayor valor predicho.
# Identificaci?n de las peliculas vistas y no vistas por el usuario 329.
# Se asume que si la pelicula no ha sido valorada es que no ha sido vista.
peliculas_vistas <- valoraciones_tidy %>%
  filter(usuario == 329 & !is.na(valoracion)) %>%
  pull(pelicula)

peliculas_no_vistas <- valoraciones_tidy %>%
  filter(usuario == 329 & is.na(valoracion)) %>%
  pull(pelicula)

# Se calcula la similitud entre cada pelicula no valorada y las si valoradas.
# Se genera un grid con todas las comparaciones que se tienen que realizar
comparaciones <- expand.grid(peliculas_no_vistas, peliculas_vistas,
                             stringsAsFactors = FALSE)
colnames(comparaciones) <- c("pelicula_no_vista", "pelicula_vista")

# Cuando un calculo implica multiples pares de vectores, suele ser practico
# almacenar los datos en forma de matriz o dataframe donde cada vector es una
# columna. Se crea un dataframe en el que cada columna es una pelicula.

atributos <- atributos %>% gather(key = "atributo", value = "valor", -title) %>%
  spread(key = title, value = valor)

# Se define la funcion que calcula la similitud
indice_jaccard <- function(pelicula1, pelicula2, datos) {
  # Esta funci?n calcula el ?ndice jaccard entre dos columnas de un dataframe.
  # El valor 1 indica presencia y el valor 0 ausencia.
  m11 <- sum(datos[, pelicula1] == 1 & datos[, pelicula2] == 1)
  m10 <- sum(datos[, pelicula1] == 1 & datos[, pelicula2] == 0)
  m01 <- sum(datos[, pelicula1] == 0 & datos[, pelicula2] == 1)
  indice <- m11 / sum(m01 + m10 + m11)
  return(indice)
}

# Con la funcion map2 del paquete purrr, se aplica la funcion indice_jaccard
# empleando las columnas del grid comparaciones como valores de los argumentos
# pelicula1 y pelicula2.
recomendaciones <- comparaciones[] %>%
  mutate(similitud = map2_dbl(.x = pelicula_no_vista,
                              .y = pelicula_vista,
                              .f = indice_jaccard,
                              datos = atributos))

# Para cada pelicula no vista, se filtran las 15 peliculas mas parecidas
recomendaciones <- recomendaciones %>% group_by(pelicula_no_vista) %>%
  top_n(n = 15, wt = similitud) %>%
  arrange(pelicula_no_vista, desc(similitud))


# Se añade la valoracion que el usuario 329 ha hecho de cada una de las peliculas
valoraciones_u329 <- valoraciones_tidy %>%
  filter(usuario == 329 & !is.na(valoracion))
recomendaciones <- recomendaciones %>%
  left_join(y = valoraciones_u329,
            by = c("pelicula_vista"  = "pelicula"))

# Media ponderada de las valoraciones por pelicula
media_ponderada <- function(df){
  resultado <- sum(df$valoracion * df$similitud) / sum(df$similitud)
  return(resultado)
}

top10_recomendaciones <- recomendaciones %>% group_by(pelicula_no_vista) %>%
  nest() %>%
  mutate(prediccion = map_dbl(.x = data,
                              .f = media_ponderada)) %>%
  select(-data) %>% arrange(desc(prediccion)) %>% head(10)
top10_recomendaciones

# Lo visualizamos
ggplot(data = top10_recomendaciones,
       aes(x = reorder(pelicula_no_vista, prediccion), y = prediccion)) +
  geom_col() +
  coord_flip() +
  labs(x = "pelicula recomendada") +
  theme_bw()

#Es importante recordar que las valoraciones han sido estandarizadas y por lo tanto, 
# tambien lo estan las predicciones. 


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   (2) Filtrado colaborativo basado en usuarios
# La estrategia seguida para recomendar 10 peliculas al usuario 329 mediante un sistema colaborativo basado 
#   en usuarios es la siguiente:
#   (2.1) Calcular la similitud entre el usuario 329 y el resto de usuarios en base a sus 
#       perfiles de valoracion, es decir, utilizando los vectores formados por sus valoraciones. 
#       Para este ejemplo se emplea la correlacion de Pearson como medida de similitud. 
#   (2.2) Identificar todas las peliculas que el usuario 329 no ha visto. Se asume que son 
#       aquellas para las que el usuario 329 no ha dado su valoracion.
#   (2.3) Para cada una de las p peliculas seleccionadas en el paso 2:
#     * Seleccionar los n=15 usuarios mas parecidos al usuario 329, cuyo valor de similitud 
#       es positivo nota2 nota3,y que si han visto la pelicula p. En la practica, el numero
#       optimo de usuarios deberia identificarse mediante validacion cruzada, sin embargo, 
#       para no añadir una capa de complejidad extra al ejemplo, se emplea este valor.
#     * Se calcula la media ponderada de las valoraciones que los n=15 usuarios han dado 
#       de la pelicula. Este valor se almacena como el valor predicho para la pelicula p.
#   (2.4) Se muestran como recomendaciones las 10 peliculas con mayor valor predicho.
# 
# Nota1: En el paso 1 del algoritmo, se calcula la similitud entre usuarios. Para que esta estimacion sea minimamente realista, 
#   conviene incluir unicamente aquellos usuarios que hayan valorado un minimo de peliculas. El valor limite se determina en 
#   funcion de los datos disponibles y de la robustez que se necesite en las estimaciones.
# Nota2: Dado que se emplea la media ponderada como estimacion final, no se pueden incluir pesos negativos. Como la correlacion 
#   de Pearson toma valores en el rango [-1, +1], se emplean unicamente aquellas observaciones con valores mayores o iguales a 
#   cero. A efectos practicos, equivale a decir que no se tienen en cuenta las valoraciones de los usuarios que tienen un perfil opuesto.
# Nota3: Aunque se establezca que se tienen que emplear los n usuarios mas similares para predecir la valoracion, puede 
#   ocurrir que, para algunas peliculas, no haya suficientes usuarios que las hayan valorado. Es conveniente recomendar unicamente 
#   peliculas cuya predicci?n est? basada en un m?nimo de usuarios, de lo contrario la estimacion puede ser muy mala. 
# 

# Se consideran unicamente aquellos usuarios que han valorado al menos 30 peliculas.
usuarios_excluidos <- valoraciones_tidy %>% filter(!is.na(valoracion)) %>%
  group_by(usuario) %>% count() %>% filter(n < 30) %>%
  pull(usuario)
valoraciones_tidy <- valoraciones_tidy %>% filter(!usuario %in% usuarios_excluidos)

# Se crea un dataframe en el que cada columna representa las valoraciones de 
# un usuario.
valoraciones_usuarios <- valoraciones_tidy %>%
  spread(key = usuario, value = valoracion, fill = NA)

# Funcion que calcula la similitud entre dos columnas
funcion_correlacion <- function(x, y){
  correlacion <- cor(x, y, use = "na.or.complete", method = "pearson")
  return(correlacion)
}

# Se aplica la funcion de correlacion a cada columna de valoraciones_usuarios,
# empleando como argumento "y" la columna del usuario "329"
similitud_usuarios <- map_dbl(.x = valoraciones_usuarios[, -1],
                              .f = funcion_correlacion,
                              y = valoraciones_usuarios[, "329"])
similitud_usuarios <- data_frame(usuario   = names(similitud_usuarios),
                                 similitud = similitud_usuarios) %>%
  arrange(desc(similitud))
head(similitud_usuarios)

# Identificacion de las peliculas vistas y no vistas por el usuario 329.
# Se asume que si la pelicula no ha sido valorada por el usuario 329 es que no
# ha sido vista.
peliculas_vistas <- valoraciones_tidy %>%
  filter(usuario == 329 & !is.na(valoracion)) %>%
  pull(pelicula)

peliculas_no_vistas <- valoraciones_tidy %>%
  filter(usuario == 329 & is.na(valoracion)) %>%
  pull(pelicula)

# Se inicia un bucle para predecir la valoracion que el usuario 329 hara de cada
#   una de las peliculas no vistas.
prediccion <- rep(NA, length(peliculas_no_vistas))
pelicula   <- rep(NA, length(peliculas_no_vistas))
n_obs_prediccion <- rep(NA, length(peliculas_no_vistas))

for(i in seq_along(peliculas_no_vistas)){
  # Usuarios que han visto la pelicula i
  usuarios_pelicula_i <- valoraciones_tidy %>%
    filter(pelicula == peliculas_no_vistas[i] & 
             !is.na(valoracion)) %>% pull(usuario)
  # Si no hay un minimo de usuarios que han visto la pelicula, no se considera una
  # estimacion suficientemente buena por lo que se pasa a la siguiente pelicula.
  if (length(usuarios_pelicula_i) < 10){
    next()
  }
  # Los 15 usuarios mas parecidos de entre los que han visto la pelicula i, cuya
  # similitud es >= 0.
  top_15_usuarios <- similitud_usuarios %>%
    filter(similitud >= 0 & (usuario %in% usuarios_pelicula_i)) %>%
    arrange(desc(similitud)) %>% 
    head(15) 
  # Si no hay un minimo de usuarios con valoraciones validas, no se considera una
  # estimacion suficientemente buena por lo que se pasa a la siguiente pelicula.
  if (nrow(top_15_usuarios) < 10){
    next()
  }
  
  # Valoraciones de esos 15 usuarios sobre la pelicula i
  valoraciones_top_15 <- valoraciones_tidy %>%
    filter(pelicula == peliculas_no_vistas[i] &
             usuario %in% top_15_usuarios$usuario)
  
  # Media ponderada de las valoraciones de los top_15_usuarios
  top_15_usuarios <- top_15_usuarios %>% left_join(valoraciones_top_15,
                                                   by = "usuario")
  prediccion[i] <- sum(top_15_usuarios$similitud * top_15_usuarios$valoracion) /
    sum(top_15_usuarios$similitud)
  pelicula[i] <- peliculas_no_vistas[i]
  n_obs_prediccion[i] <- nrow(top_15_usuarios)
}

top10_recomendaciones <- data.frame(pelicula, prediccion, n_obs_prediccion) %>% 
  arrange(desc(prediccion)) %>%
  head(10)
top10_recomendaciones

# La columna n_obs_prediccion contiene el numero de usuarios que se han empleado para estimar la 
#   valoracion de la pelicula. Es importante tenerlo en cuenta ya que, aunque por defecto son 15, 
#   puede ocurrir que para algunas peliculas no haya tantos usuarios que las hayan valorado.
ggplot(data = top10_recomendaciones,
       aes(x = reorder(pelicula, prediccion), y = prediccion)) +
  geom_col() +
  coord_flip() +
  labs(x = "pelicula recomendada") +
  theme_bw()


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   (3) Filtrado colaborativo basado en items

# La estrategia seguida para recomendar 10 peliculas al usuario 329 mediante un sistema colaborativo basado 
#   en items es la siguiente:
#   
#   (3.1) Identificar todas las peliculas que el usuario 329 no ha visto. Se asume que son aquellas para las que 
#       el usuario 329 no ha dado su valoracion.
#   (3.2) Para cada una de las p peliculas seleccionadas en el paso 1:
#         * Calcular la similitud con las peliculas que el usuario 329 se ha visto, en base al perfil de valoracion 
#           que han recibido, es decir, utilizando los vectores formados por sus valoraciones. Para este ejemplo se 
#           emplea la correlacion de Pearson como medida de similitud. nota1
#         * Seleccionar los n=15 peliculas mas parecidas. En la practica, el numero optimo de peliculas deberia 
#           identificarse mediante validacion cruzada, sin embargo, para no añadir una capa de complejidad extra al ejemplo, 
#           se emplea este valor. nota2
#         * Se calcula la media ponderada de las valoraciones que el usuario 329 ha hecho de las n=15 peliculas m?s 
#           parecidas. Este valor se almacena como el valor predicho para la pelicula p. nota3
# 
# Se muestran como sugerencias las 10 peliculas con mayor valor de predicci?n.
# 
# Nota1: En el paso 1 del algoritmo, se calcula la similitud entre peliculas. Para que esta estimaci?n sea minimamente realista, 
#   conviene incluir unicamente aquellas peliculas que hayan sido valoradas por un minimo de usuarios. El valor limite se 
#   determina en funcion de los datos disponibles y de la robustez que se necesite en las estimaciones.
# 
# Nota2: Dado que se emplea la media ponderada como estimaci?n final, no se pueden incluir pesos negativos. Como la correlacion 
#   de Pearson toma valores en el rango [-1, +1], se emplean ?nicamente aquellas observaciones con valores mayores o iguales a 
#   cero. A efectos pr?cticos, equivale a decir que no se tienen en cuenta las valoraciones de los usuarios que tienen un perfil opuesto.
# 
# Nota3: Aunque se establezca que se tienen que emplear las n peliculas mas similares para predecir la valoracion, puede ocurrir que 
#   no haya suficientes. Es conveniente hacer recomendaciones basadas en un m?nimo de observaciones, de lo contrario la estimaci?n puede ser muy mala. 

# Para que el calculo de similitudes entre peliculas sea valido, se emplean unicamente peliculas que hayan recibido un minimo de 10 valoraciones.
valoraciones_tidy <- valoraciones %>% gather(key = "pelicula",
                                             value = "valoracion",
                                             -usuario) %>%
  group_by(usuario) %>% 
  mutate(valoracion = scale(valoracion)) %>%
  ungroup()
peliculas_excluidas <- valoraciones_tidy %>% filter(!is.na(valoracion)) %>%
  group_by(pelicula) %>% count() %>% filter(n < 5) %>%
  pull(pelicula)
valoraciones_tidy <- valoraciones_tidy %>%
  filter(!pelicula %in% peliculas_excluidas)
# Identificacion de las peliculas vistas y no vistas por el usuario 329.
# Se asume que si la pelicula no ha sido valorada es que no ha sido vista.
peliculas_vistas <- valoraciones_tidy %>%
  filter(usuario == 329 & !is.na(valoracion)) %>%
  pull(pelicula)

peliculas_no_vistas <- valoraciones_tidy %>%
  filter(usuario == 329 & is.na(valoracion)) %>%
  pull(pelicula)

# Se genera un grid con todas las comparaciones que se tienen que realizar
comparaciones <- expand.grid(peliculas_no_vistas, peliculas_vistas,
                             stringsAsFactors = FALSE)
colnames(comparaciones) <- c("pelicula_no_vista", "pelicula_vista")

# Se crea un dataframe en el que cada columna es una pelicula
valoraciones <- valoraciones_tidy %>%
  spread(key = pelicula, value = valoracion, fill = NA)

# Se define la funci?n que calcula la similitud
correlacion <- function(pelicula1, pelicula2, datos) {
  # Esta funcion calcula la correlaci?n entre dos columnas de un dataframe.
  similitud <- cor(x = datos[, pelicula1], y = datos[, pelicula2],
                   method = "pearson", use = "na.or.complete")
  return(similitud)
}

# Con la funcion map2 del paquete purrr, se aplica la funci?n correlacion empleando
# las columnas del grid comparaciones como valores de los argumentos pelicula1 y
# pelicula2.

comparaciones <- comparaciones %>%
  mutate(similitud = map2_dbl(.x = pelicula_no_vista,
                              .y = pelicula_vista,
                              .f = correlacion,
                              datos = valoraciones))

# Para cada pelicula no vista, se filtran las 15 peliculas mas parecidas y cuyo 
# valor de similitud es mayor o igual a cero.
comparaciones <- comparaciones %>% filter(similitud >= 0) %>%
  group_by(pelicula_no_vista) %>%
  top_n(n = 15, wt = similitud) %>%
  arrange(pelicula_no_vista, desc(similitud))

# Se eliminan aquellas peliculas para las que no haya un minimo de peliculas 
# similares con valores positivos.
exclusion <- comparaciones %>%
  group_by(pelicula_no_vista) %>%
  count() %>%
  filter(n < 10) %>%
  pull(pelicula_no_vista)
comparaciones <- comparaciones %>% filter(!pelicula_no_vista %in% exclusion)

# Se añade la valoracion que el usuario 329 ha hecho de cada una de las peliculas.
valoraciones_u329 <- valoraciones_tidy %>%
  filter(usuario == 329 & !is.na(valoracion))
comparaciones <-  comparaciones %>%
  left_join(y = valoraciones_u329,
            by = c("pelicula_vista"  = "pelicula"))

# Media ponderada de las valoraciones por pelicula
media_ponderada <- function(df){
  resultado <- sum(df$valoracion * df$similitud) / sum(df$similitud)
  return(resultado)
}

top10_recomendaciones <- comparaciones %>% group_by(pelicula_no_vista) %>%
  nest() %>%
  mutate(prediccion = map_dbl(.x = data,
                              .f = media_ponderada)) %>%
  select(-data) %>% arrange(desc(prediccion)) %>% head(10)
top10_recomendaciones

# Las visualizamos
ggplot(data = top10_recomendaciones,
       aes(x = reorder(pelicula_no_vista, prediccion), y = prediccion)) +
  geom_col() +
  coord_flip() +
  labs(x = "pelicula recomendada") +
  theme_bw()









