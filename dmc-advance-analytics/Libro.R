###################################################
##### DMC : Mineria de textos                 #####
##### Analisis de libros                      #####
##### Profesor: Daniel Chavez Gallo           #####
###################################################

# Antes de nada, limpiamos el workspace, por si hubiera algun dataset o informacion cargada
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Limpiamos la consola
cat("\014")

# Dado que vamos a tener bastantes problemas de codificación de caracteres, lo fijamos a UTF-8
options(encoding = "utf-8")

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
library(cvTools)

# Vamos a leer el libro
texto <- pdftools::pdf_text("Datos/manualResistencia.pdf")
texto

# Vamos a hacer un poco de limpieza de texto
texto <- gsub("\\r", " ", texto)
texto <- gsub("\\n", "", texto)
texto <- gsub("\\d\\K\\.(?=\\d)", "", texto, perl = TRUE)#  Los puntos de separador de mil, lo sustituimos por un espacio

# Juntamos todas las páginas del libros
texto<-paste(texto, collapse = '')
texto

# Vamos a estructurar el texto en frases (entendiendo por "frase" una separación con .)
vector = c()
for(i in 1:length(texto)){
  temp<-(strsplit(texto[[i]], "\\.")[[1]])
  print(temp)
  vector <- c(vector, temp)
}
# Lo convertimos a un dataframe para ver cómo queda
frases_texto<-as.data.frame(vector)

# Ahora, hacemos un poco de limpieza manual (se podría automatizar algo más...)
#   1. Quitamos las páginas del índice
frases_texto<-as.data.frame(frases_texto[22:nrow(frases_texto),])
#   2. Quitamos el epílogo y todo lo que viene al final
frases_texto<-as.data.frame(frases_texto[1:4261,])
#   3. Renombramos la columna para dejar todo listo
colnames(frases_texto)[1]<-"frase"
#   4. Quitamos los espacios al comienzo de capítulos y demás
frases_texto$frase<-trimws(frases_texto$frase, "l")
#   5. Convertimos a carácter para poder hacer su análisis
frases_texto$frase<-as.character(frases_texto$frase)

###################################################################################################
###################################################################################################
# Empezamos el análisis
###################################################################################################
###################################################################################################
###################################################################################################
# 0. Análisis exploratorio de datos
###################################################################################################
###################################################################################################
# Nos creamos un lexicon de stopwords en español a medida para el caso del libro de Pedro Sánchez
lexiconSW<-stopwords("es")
lexiconSW<-append(lexiconSW,c(""))
lexiconSW<-as.data.frame(lexiconSW)
names(lexiconSW)<-"word"
lexiconSW$word<-as.character(lexiconSW$word)

# 1.1. Algunos análisis básicos
df <- tibble::rowid_to_column(frases_texto, "ID")
# Quitamos las stopwords
review_words <- df %>%
  distinct(frase, .keep_all = TRUE) %>%
  unnest_tokens(word, frase, drop = FALSE) %>%
  distinct(ID, word, .keep_all = TRUE) %>%
  anti_join(lexiconSW) %>%
  filter(str_detect(word, "[^\\d]")) %>%
  group_by(word) %>%
  dplyr::mutate(word_total = n()) %>%
  ungroup()
# Contamos las palabras resultantes
word_counts <- review_words %>%
  dplyr::count(word, sort = TRUE)

word_counts %>%
  head(40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "blue") +
  scale_y_continuous(labels = comma_format()) +
  coord_flip() +
  labs(title = paste0("Palabras más utilizadas"),
       subtitle = "Stopwords retiradas",
       x = "Palabra",
       y = "Número de veces usada")

# 1.2. Bigramas
# A veces nos interesa entender la relación entre palabras en una opinión. 
review_bigrams <- df %>%
  unnest_tokens(bigram, frase, token = "ngrams", n = 2)
bigrams_separated <- review_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% lexiconSW$word) %>%
  filter(!word2 %in% lexiconSW$word)
bigram_counts <- bigrams_filtered %>% 
  dplyr::count(word1, word2, sort = TRUE)
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united %>%
  dplyr::count(bigram, sort = TRUE)

# Podemos visualizarlo también
review_subject <- df %>% 
  unnest_tokens(word, frase) %>% 
  anti_join(lexiconSW)
my_stopwords <- data_frame(word = c(as.character(1:10)))
review_subject <- review_subject %>% 
  anti_join(my_stopwords)
title_word_pairs <- review_subject %>% 
  pairwise_count(word, ID, sort = TRUE, upper = FALSE)
# Nos generamos el listado de bigramas
listadoBigramas<-title_word_pairs[which(title_word_pairs$n>10),]
set.seed(1234)
title_word_pairs %>%
  filter(n >= 14) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle('Bigramas')

# 1.3. Análisis de trigramas
review_trigrams <- df %>%
  unnest_tokens(trigram, frase, token = "ngrams", n = 3)
trigrams_separated <- review_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% lexiconSW$word) %>%
  filter(!word2 %in% lexiconSW$word) %>%
  filter(!word3 %in% lexiconSW$word)
trigram_counts <- trigrams_filtered %>% 
  dplyr::count(word1, word2, word3, sort = TRUE)
trigrams_united <- trigrams_filtered %>%
  unite(trigram, word1, word2, word3, sep = " ")
trigrams_united %>%
  dplyr::count(trigram, sort = TRUE)

###################################################################################################
###################################################################################################
# 1. Análisis clúster
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
# Funciones para seleccionar el número de clústers óptimo
###################################################################################################
###################################################################################################
cvLDA <- function(Ntopics,dtm,K=10) {
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

numeroTopicsOptimo<-function(dtm){
  K <- c(5,10,20, 30, 40, 50, 60, 70, 80)
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
  return(pq)
}

# Función de análisis clúster
corpus <- corpus(frases_texto$frase)
cdfm <- dfm(corpus, remove=c(stopwords("spanish"), 
                             ""), 
            verbose=TRUE, remove_punct=TRUE, remove_numbers=TRUE)
cdfm <- dfm_trim(cdfm, min_docfreq = 2, verbose=TRUE)
# Vamos a hacer una nube de palabras
textplot_wordcloud(cdfm, rot.per=0, scale=c(5.5, 1.25), max.words=200)
# Ahora lo exportamos a un formato para procesar los Topic Models.
dtm <- convert(cdfm, to="topicmodels")
# Seleccionando el número de topics óptimo
vis<-numeroTopicsOptimo(dtm)
vis


# Obtenemos el LDA con el número óptimo de topics que nos haya salido
lda <- LDA(dtm, k = 30, method = "Gibbs",
             control = list(verbose=25L, seed = 123, burnin = 100, iter = 500))
  
# Obtenemos la palabra más representativa de cada topic
terms(lda)
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
terminosTopic <- tidy(lda, matrix = "beta")
terminosTopic
  
# Sacamos los top 20 términos por cada topic
top_terms <- terminosTopic %>%
  group_by(topic) %>%
  top_n(12, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
  
  # Los visualizamos
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()+
  labs(x = NULL, y = "Importancia palabras en topic",
       title = paste0("Topics y sus palabras descriptivas"))

