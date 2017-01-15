

narediKorpus <- function(){
  corpus <-Corpus(DirSource("./textsModifiedNum"))
  
  # Predobdelava kopusa
  corpus  <- tm_map(corpus , removeNumbers)
  corpus  <- tm_map(corpus , removePunctuation)
  corpus  <- tm_map(corpus , content_transformer(tolower))
  corpus  <- tm_map(corpus , removeWords, stopwords())
  corpus  <- tm_map(corpus , stemDocument)
  corpus  <- tm_map(corpus , stripWhitespace)
  corpus
}