
###################################################
##### DMC : Deep Learning                     #####
##### Metodos h2o - Modelos                   #####
##### Profesor: Daniel Chavez Gallo           #####
###################################################

# Limpiar memoria, eliminar objetos
rm(list = ls())

# Directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# El problema que queremos resolver es clasificar unas imágenes 
#   en escala de grises manuscritas. Esas imágenes se describen formalmente por:
#     - Input: Dígitos expresados en una matriz de 28x28 píxeles.
#     - Output: Clasificador multicategoría de 0 a 9, los dígitos índicos.

# Vamos a usar el dataset MNIST: 60.000 imágenes de entrenamiento + 10.000 de test.
#   Train_images y Train_labels conforman el dataset de entrenamiento.
#   Test_images y Test_labels conforman el dataset de validación.

library(h2o)
localH2O = h2o.init(max_mem_size = "8g", nthreads = -1)

## Import MNIST CSV as H2O
train <- h2o.importFile("/home/diego/Documents/notebooks/dmc-advance-analytics/data/mnist_train.csv")
h2o.dim(train)
test <- h2o.importFile("/home/diego/Documents/notebooks/dmc-advance-analytics/data/mnist_test.csv")
h2o.dim(test)

# Se observa que hay 784 pixeles en cada imagen. Esto sugiere un conjunto de datos de entrada muy grande. 
# Ahora, tenemos un espacio de parámetros mucho más grande que debe ajustarse a la red de aprendizaje profundo. 
# Usamos un modelo de tres capas ocultas, y cada capa oculta tiene 10 nodos.
y <- "label"
x <- setdiff(names(train), y)
train[,y] <- h2o.asfactor(train[,y])
test[,y] <- h2o.asfactor(test[,y])

# El modelo
model <- h2o.deeplearning(x = x,
                          y = y,
                          training_frame = train,
                          validation_frame = test,
                          distribution = "multinomial", # por la cantidad de categorias en el objetivo
                          activation = "RectifierWithDropout", # ideal para objetivos cualitativos
                          hidden = c(10,10,10), # 3 capas ocultas, cada una con 10 nodos
                          input_dropout_ratio = 0.2, # índice de abandono de la capa de entrada 
                                                     # (puede mejorar la generalización, 
                                                     # pruebe con 0,1 o 0,2). El valor predeterminado es 0.
                          l1 = 1e-5, # Regularización L1 (puede agregar estabilidad y mejorar la generalización,
                                     # hace que muchos pesos se conviertan en 0). El valor predeterminado es 0.
                          epochs = 20 # La cantidad de veces que se debe iterar (transmitir) el conjunto de datos
                                      # puede ser fraccional. El valor predeterminado es 10.
                          )

# El error medio es mucho mayor, alrededor de un tercio. Parece que el error más alto surge 
# de que el DLN confunde el número "8" con el número "1". También parece confundir el número "3"
# con el número "5". Sin embargo, parece que lo hace mejor para identificar los números "3" y "7".

# Repetimos el modelo con una red más profunda con más nodos para ver si aumenta la precisión.

model <- h2o.deeplearning(x = x,
                          y = y,
                          training_frame = train,
                          validation_frame = test,
                          distribution = "multinomial",
                          activation = "RectifierWithDropout",
                          hidden = c(50,50,50,50,50), # 5 capas ocultas, cada una con 50 nodos
                          input_dropout_ratio = 0.2,
                          l1 = 1e-5,
                          epochs = 20)

# Ahora la tasa de error se reduce considerablemente. 
# La mejora proviene de más nodos en cada capa o de más capas ocultas?

model <- h2o.deeplearning(x = x,
                          y = y,
                          training_frame = train,
                          validation_frame = test,
                          distribution = "multinomial",
                          activation = "RectifierWithDropout",
                          hidden = c(100,100,100),
                          input_dropout_ratio = 0.2,
                          l1 = 1e-5,
                          epochs = 20)


h2o.removeAll()
h2o.shutdown()











