#install.packages(c("tm", "SnowballC", "wordcloud", "proxy", "kernlab", "NLP", "openNLP")) 
#install.packages("openNLPmodels.en", repos="http://datacube.wu.ac.at/", type="source")

# Nalozimo knjiznico za tekstovno rudarjenje v sistemu R
library(tm)
library(NLP)
library(openNLP)
library(openNLPmodels.en)

source("corpus.R")

corpus <- narediKorpus()


########################################
#  4. Preoblikovanje korpusa v ucno mnozico
########################################
#
# Zgradimo matriko pojavitev dokument-beseda
data.tfidf <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))
#Odstranimo redke besede? 
#data.tfidf  <- removeSparseTerms(data.tfidf , sparse=0.4)

#TODO razširitev atributnega prostora
  #število različnih besed v dokumentu
  #število redkih besed v dokumentu
  #povprečno število stavkov v dokumentu
  #povprečno število atributov na stavek

  # Ciljna spremenljivka
Topic <- as.data.frame(read.csv("textiFiction.csv")[ ,5])


# Zdradimo ucno mnozico tako, da matriki pojavitev dokument-beseda dodamo stolpec s tematiko dokumentov (razred)
data <- cbind(as.matrix(data.tfidf), Topic)
names(data)[ncol(data)] <- "Topic"

# Ucno mnozico razdelimo na dejansko ucno mnozico in testno mnozico
sel <- sample(nrow(data), 200, F)
train <- data[-sel,]
test <- data[sel,]

############################################
# 5. KLASIFIKACIJA
#############################################





