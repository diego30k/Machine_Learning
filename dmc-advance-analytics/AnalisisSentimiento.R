###################################################
##### DMC : Mineria de textos                 #####
##### Analisis de sentimientos                #####
##### Profesor: Daniel Chavez Gallo           #####
###################################################

# Antes de nada, limpiamos el workspace, por si hubiera algún dataset o información cargada
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Limpiamos la consola
cat("\014")

# Cargamos las librerías que vamos a necesitar
library(tidytext)
library(textdata)
library(syuzhet) #  Librería para emociones
library(dplyr)
library(ggplot2)
library(stopwords)
library(tm)
library(tidyverse)
library(wordcloud)

# Leemos los datos
datos <- read_csv("Datos/recibos.csv")

# 1. Clasificador de sentimiento
# Vamos a utilizar 3 lexicon para la clasificación de sentimiento
#   afinn: asigna palabras con una puntuación (entre -5 y 5) con puntuaciones negativas que indican un sentimiento negativo
#   bing: clasifica las palabras en categorías positivas y negativas
#   nrc: clasifica las palabras en categorías positivas y negativas, así como por tipo de sentimiento
# (ira, anticipación, asco, miedo, alegría, tristeza, sorpresa y confianza)
afinn=get_sentiments("afinn") 
bing=get_sentiments("bing")

# Análisis de tipos de emoción / sentimiento de cada discurso
#   Fusiono bing y rc para aumentar el espectro de palabras
emocion=rbind(bing)
# Elimino las duplicadas porque ambos lexicons ofrecen valoración positive/negative
emocion=unique(emocion) 

#   análisis de sentimiento, o sea que la limpieza se hará sola
palabras <- datos%>%unnest_tokens(word, mensaje) 

# Cruzo los mensajes de cada tema con los diccionarios para valorar todas las palabras 
#   de los discursos que aparezcan
analisis_emociones=merge(palabras,emocion,by="word")
# Meto un contador para poder visualizar de manera gráfica de una forma más fácil
analisis_emociones$count=1

# Acumulo el número de palabras de cada tipo de emoción de cada discurso
emo=aggregate(count~sentiment, data=analisis_emociones, FUN=sum)
emo$sentiment=as.factor(emo$sentiment)

ggplot(emo, aes(factor(sentiment), count)) + 
  geom_bar(stat="identity", position = "dodge") +
  labs(title = paste0("Palabras más utilizadas"),
       subtitle = "Un mensaje puede provocar más de un sentimiento",
       x = "Sentimiento",
       y = "Número de mensajes con sentimiento")

# Vamos a coger las palabras en español del diccionario NRC
nrc <- get_sentiment(datos$mensaje, method="nrc",lang="spanish")
#El método predeterminado, "syuzhet" es un diccionario de sentimientos personalizado 
# desarrollado en el Laboratorio Literario de Nebraska. 165,000 oraciones codificadas 
# por humanos tomadas de un pequeño corpus de novelas contemporáneas. Esto significa 
# efectivamente que "árabe", "bengalí", "simplificado en chino", "tradicional chino", 
# "griego", "gujarati", "hebreo", "hindi", "japonés", "marathi", "persa", "ruso "," 
# Tamil "," Telugu "," Thai "," Ukranian "," Urdu "," Yiddish "no son compatibles a 
# pesar de que estos idiomas son parte del diccionario extendido de NRC

# Obtenemos las emociones
emotions <- get_nrc_sentiment(datos$mensaje,lang="spanish")
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

# Comparacion word cloud
all = c(
  paste(datos$mensaje[emotions$anger > 0], collapse=" "),
  paste(datos$mensaje[emotions$anticipation > 0], collapse=" "),
  paste(datos$mensaje[emotions$disgust > 0], collapse=" "),
  paste(datos$mensaje[emotions$fear > 0], collapse=" "),
  paste(datos$mensaje[emotions$joy > 0], collapse=" "),
  paste(datos$mensaje[emotions$sadness > 0], collapse=" "),
  paste(datos$mensaje[emotions$surprise > 0], collapse=" "),
  paste(datos$mensaje[emotions$trust > 0], collapse=" ")
)
all <- removeWords(all, stopwords("spanish"))
corpus = Corpus(VectorSource(enc2native(all)))

my_tokenizer <- function (x) {
  strsplit(iconv(x, to='UTF-8'), split='([[:space:]]|[[:punct:]])+', perl=F)[[1]]
}
?iconv
tdm <- TermDocumentMatrix(corpus,control=list(tokenize=my_tokenizer))
tdm

# convert as matrix
tdm = as.matrix(tdm)
tdm
colnames(tdm) = c('Enfado', 'Anticipación', 'Disgusto', 'Miedo', 'Alegría', 'Tristeza', 'Sorpresa', 'Confianza')
par(mar = rep(0, 4))

comparison.cloud(tdm, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=150, scale=c(2.0, 0.4),rot.per=0.2)



