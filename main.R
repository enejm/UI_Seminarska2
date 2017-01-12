#install.packages(c("tm", "SnowballC", "wordcloud", "proxy", "kernlab", "NLP", "openNLP")) 


# Nalozimo knjiznico za tekstovno rudarjenje v sistemu R
library(tm)

corpus <-Corpus(DirSource("textmining/economy"))

length(corpus )


# Predobdelava kopusa
corpus  <- tm_map(corpus , removeNumbers)
corpus  <- tm_map(corpus , removePunctuation)
corpus  <- tm_map(corpus , content_transformer(tolower))
corpus  <- tm_map(corpus , removeWords, stopwords())
corpus  <- tm_map(corpus , stemDocument)
corpus  <- tm_map(corpus , stripWhitespace)
