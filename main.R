install.packages(c("tm", "SnowballC", "wordcloud", "proxy", "kernlab", "NLP", "openNLP")) 


# Nalozimo knjiznico za tekstovno rudarjenje v sistemu R
library(tm)

source("corpus.R")

corpus <- narediKorpus()



##TODO
#
# Preoblikovanje korpusa v ucno mnozico
#

# Zgradimo matriko pojavitev dokument-beseda
data.tfidf <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))

# Preberemo podatke o tematiki dokumentov, ki sluzijo kot razredi v klasifikaciji
Topic <- as.data.frame(read.csv("textiFiction.csv")[ ,5])
#Topic <-read.table("textmining/economy-topics.txt")

# Zdradimo ucno mnozico tako, da matriki pojavitev dokument-beseda dodamo stolpec s tematiko dokumentov (razred)
data <- cbind(as.matrix(data.tfidf), Topic)
names(data)[ncol(data)] <- "Topic"

# Ucno mnozico razdelimo na dejansko ucno mnozico in testno mnozico
sel <- sample(nrow(data), 200, F)
train <- data[-sel,]
test <- data[sel,]


