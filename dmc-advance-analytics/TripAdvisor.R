###################################################
##### DMC : Mineria de textos                 #####
##### Ejercicio TripAdvisor                   #####
##### Profesor: Daniel Chavez Gallo           #####
###################################################

install.packages(c("wordcloud", "textdata","syuzhet",
                   "tidytext", "stopwords","tm",                                   
                   "wordcloud", "pdftools","stringi",
                   "stringr", "scales","widyr",
                   "ggraph", "igraph","quanteda","topicmodels",
                   "cvTools", "SnowballC"), dependencies = T)

# Antes de nada, limpiamos el workspace, por si hubiera algun dataset o informacion cargada
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Limpiamos la consola
cat("\014")

# Vamos a cargar las librerías necesarias
library(pdftools)
library(dplyr)
library(stopwords)
library(tidytext)
library(stringi)
library(stringr)
library(ggplot2)
library(scales)
library(tidyr)
library(widyr)
library(ggraph)
library(igraph)
library(quanteda)
library(topicmodels)
library(cvTools) #  Para el cálculo de topics
library(lubridate) # Para el redondeo de fechas
library(SnowballC) #  Para hacer stemming

##########################################################################
# 1. Leemos las opiniones para empezar con el ejercicio
##########################################################################
opiniones<-read.csv("data/opinionesPrado.csv")
# opiniones<-read.csv("Datos/opinionesGuggenheim.csv")
opiniones
# Mejoramos un poco la calidad de los datos
opiniones=opiniones[,c(3:4)]
colnames(opiniones)[1]<-"review_body"
str(opiniones)
opiniones$review_body=as.character(opiniones$review_body)
opiniones$review_date=as.Date(opiniones$review_date)
str(opiniones)

opiniones %>%
  dplyr::count(Week = round_date(review_date, "week")) %>% # por semana
  ggplot(aes(Week, n)) +
  geom_line() + 
  ggtitle('El numero de comentarios por semana')

# 1.1. Algunos análisis básicos
df <- tibble::rowid_to_column(opiniones, "ID") # añadiendo un ID
df <- df %>%
        mutate(review_date = as.POSIXct(review_date, origin = "1970-01-01"),
               month = round_date(review_date, "month"))
?rowid_to_column

# Nos creamos un lexicon de stopwords en español a medida
custom_stop_words <- stopwords("es")
custom_stop_words <- append(custom_stop_words, c("má","museo","más","mã",""," ")) # adicionando temas fijos
custom_stop_words <- as.data.frame(custom_stop_words)
names(custom_stop_words) <- "word"
custom_stop_words$word <- as.character(custom_stop_words$word)

# Sin quitar stop_words
distintos <- df %>%
  distinct(review_body, .keep_all = TRUE) # verificar tamaño


df.1 <- df[1:10,]
dim(df)
length(custom_stop_words$word)
review_words <- df %>%
  distinct(review_body, .keep_all = TRUE) %>% # si es FALSE no ejecuta la eliminacion de repetidos
  unnest_tokens(word, review_body, drop = FALSE) %>% # crea una columnas con los tokens de cada opinion
  distinct(ID, word, .keep_all = TRUE) %>% # elimina repetidos
  filter(str_detect(word, "[^\\d]")) %>% # Expresion regular para negar la extraccion de letras
  group_by(word) %>%
  mutate(word_total = n()) %>%
  ungroup()



# Quitando stopwords
review_words <- df %>%
  distinct(review_body, .keep_all = TRUE) %>%
  unnest_tokens(word, review_body, drop = FALSE) %>%
  distinct(ID, word, .keep_all = TRUE) %>%
  anti_join(custom_stop_words) %>%   # quito los stop_word
  filter(str_detect(word, "[^\\d]")) %>%
  group_by(word) %>%
  mutate(word_total = n()) %>%
  ungroup()

word_counts <- review_words %>%
  count(word, sort = TRUE)

# Formateamos un poco la gráfica
word_counts %>%
  head(25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "blue") +
  scale_y_continuous(labels = comma_format()) +
  coord_flip() +
  labs(title = "Palabras + usadas Museo del Prado-TripAdvisor(2005-18)",
       subtitle = "29061 opiniones; stopwords retiradas", x = "Palabra",
       y = "Número de veces usada")

# Y ahora haciendo stemming: 
word_counts %>%
  head(25) %>%
  mutate(word = wordStem(word)) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "blue") +
  scale_y_continuous(labels = comma_format()) +
  coord_flip() +
  labs(title = "Palabras más usadas para describir Museo del Prado en TripAdvisor (2005-2018)",
       subtitle = "29061 opiniones; stopwords retiradas y stemming", x = "Palabra",
       y = "Número de veces usada")

# 1.2. Bigramas
# A veces nos interesa entender la relación entre palabras en una opinión. 
review_bigrams <- df %>%
  unnest_tokens(bigram, review_body, token = "ngrams", n = 2)
bigrams_separated <- review_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united %>%
  count(bigram, sort = TRUE)

# Podemos visualizarlo también
review_subject <- df %>% 
  unnest_tokens(word, review_body) %>% 
  anti_join(custom_stop_words)
my_stopwords <- data_frame(word = c(as.character(1:10)))
review_subject <- review_subject %>% 
  anti_join(my_stopwords)
title_word_pairs <- review_subject %>% 
  pairwise_count(word, ID, sort = TRUE, upper = FALSE)
set.seed(1234)
title_word_pairs %>%
  filter(n >= 150) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle('Red de palabras Museo del Prado - TripAdvisor')
theme_void()

# Análisis clúster

corpus <- corpus(opiniones$review_body)
cdfm <- dfm(corpus, remove=c(stopwords("spanish"), 
                             "t.co", "https", "museo","prado", 
                             "rt","amp", "españa","españoles", 
                             "t","i", "http", "t.c", "can"), 
            verbose=TRUE, remove_punct=TRUE, remove_numbers=TRUE)
cdfm <- dfm_trim(cdfm, min_docfreq = 2, verbose=TRUE)

# Ahora lo exportamos a un formato para procesar los Topic Models.
dtm <- convert(cdfm, to="topicmodels")

## Seleccionando el número de topics óptimo
# Estimamos el LDA con el número óptimo de topics que nos haya salido
K <- 20
lda <- LDA(dtm, k = K, method = "Gibbs", 
           control = list(verbose=25L, seed = 123, burnin = 100, iter = 500))
save(lda, file="ldaPrado.Rdata")

# Obtenemos la palabra más representativa de cada topic
terms(lda)
?`terms,TopicModel-method`
# Las top 10-palabras de cada topic
trms <- t(terms(lda, k=10))
# Algunos topics son fáciles de identificar
trms[6,]
trms[1,] 
trms[17,]
trms[22,]
trms[29,] 
trms[13,]
trms[24,]
trms[16,]
trms[9,]
trms[1,]
trms[29,]

# Sacamos los datos de cada topic
terminosPrado <- tidy(lda, matrix = "beta")
terminosPrado
?tidy
# Sacamos los top 8 términos por cada topic
top_terms <- terminosPrado %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

# Los visualizamos
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

## Seleccionando el número de topics óptimo
# install.packages("cvTools")
require(cvTools)
cvLDA <- function(Ntopics, dtm,K=10) {
  folds<-cvFolds(nrow(dtm),K,1)
  perplex <- rep(NA,K)
  llk <- rep(NA,K)
  for(i in unique(folds$which)){
    cat(i, " ")
    which.test <- folds$subsets[folds$which==i]
    which.train <- {1:nrow(dtm)}[-which.test]
    dtm.train <- dtm[which.train,]
    dtm.test <- dtm[which.test,]
    lda.fit <- LDA(dtm.train, k=Ntopics, method="Gibbs",
                   control=list(verbose=50L, iter=100))
    perplex[i] <- perplexity(lda.fit,dtm.test)
    llk[i] <- logLik(lda.fit)
  }
  return(list(K=Ntopics,perplexity=perplex,logLik=llk))
}


K <- c(20, 30, 40, 50, 60, 70, 80)
results <- list()

i = 1
for (k in K){
  cat("\n\n\n##########\n ", k, "topics", "\n")
  res <- cvLDA(k, dtm)
  results[[i]] <- res
  i = i + 1
}

## plot
df <- data.frame(
  k = rep(K, each=10),
  perp =  unlist(lapply(results, '[[', 'perplexity')),
  loglk = unlist(lapply(results, '[[', 'logLik')),
  stringsAsFactors=F)

min(df$perp)
df$ratio_perp <- df$perp / max(df$perp)
df$ratio_lk <- df$loglk / min(df$loglk)

df <- data.frame(cbind(
  aggregate(df$ratio_perp, by=list(df$k), FUN=mean),
  aggregate(df$ratio_perp, by=list(df$k), FUN=sd)$x,
  aggregate(df$ratio_lk, by=list(df$k), FUN=mean)$x,
  aggregate(df$ratio_lk, by=list(df$k), FUN=sd)$x),
  stringsAsFactors=F)
names(df) <- c("k", "ratio_perp", "sd_perp", "ratio_lk", "sd_lk")
library(reshape)
pd <- melt(df[,c("k","ratio_perp", "ratio_lk")], id.vars="k")
pd2 <- melt(df[,c("k","sd_perp", "sd_lk")], id.vars="k")
pd$sd <- pd2$value
levels(pd$variable) <- c("Perplexity", "LogLikelihood")

library(ggplot2)
library(grid)

p <- ggplot(pd, aes(x=k, y=value, linetype=variable))
pq <- p + geom_line() + geom_point(aes(shape=variable),
                                   fill="white", shape=21, size=1.40) +
  geom_errorbar(aes(ymax=value+sd, ymin=value-sd), width=4) +
  scale_y_continuous("Ratio wrt worst value") +
  scale_x_continuous("Number of topics",
                     breaks=K) +
  theme_bw()
pq

## Seleccionando el número de topics óptimo
# Análisis predictivo

# Vamos a tratar de hacer un predictivo de cuando usen la palabra "Perú"
opiniones$visit <- ifelse(grepl("visitar", opiniones$review_body), 1, 0)
prop.table(table(opiniones$visit)) # Hay bastante buen balanceo de clases

# Limpiamos los plots
dev.off()

# Creamos una parte de Train y otra de Test, con 80-20
set.seed(123)
training <- sample(1:nrow(opiniones), floor(.80 * nrow(opiniones)))
test <- (1:nrow(opiniones))[1:nrow(opiniones) %in% training == FALSE]





# Construimos el DFM
twcorpus <- corpus(opiniones$review_body)
twdfm <- dfm(twcorpus, remove_punct=TRUE, remove_numbers=TRUE, remove=c(
  stopwords("spanish"), "t.co", "https", "prado","rt", "amp", "http", "t.c", "can","m","p"))
twdfm <- dfm_trim(twdfm, min_docfreq = 3)
textplot_wordcloud(twdfm, rot.per=0, scale=c(3.5, .75), max.words=100)

# Now run the classifier. Then, compute the accuracy.
library(glmnet)
ridge <- cv.glmnet(twdfm[training,], opiniones$visit[training], 
                   family="binomial", alpha=0, nfolds=5, parallel=TRUE,
                   type.measure="deviance")
plot(ridge)
## Accuracy
accuracy <- function(ypred, y){
  tab <- table(ypred, y)
  return(sum(diag(tab))/sum(tab))
}

# Valores que se preducen
preds <- predict(ridge, twdfm[test,], type="response") > mean(opiniones$visit[test])
preds
# Matriz de confusión
table(preds, opiniones$visit[test])
# Métricas de rendimiento
accuracy(preds, opiniones$visit[test])

# Identificamos las características que mejor predicen
best.lambda <- which(ridge$lambda==ridge$lambda.min)
beta <- ridge$glmnet.fit$beta[,best.lambda]
head(beta,n=30)
beta
