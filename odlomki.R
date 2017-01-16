narediKorpusOdlomkov <- function(){
  corpus <-Corpus(DirSource("./textsModifiedOdlomki"))
  
  # Odstranjevanje lastnih imen
  conn <- file("lastnaImenaStopwords", open="r")
  lastnaImenaSW <- readLines(conn)
  lastnaImenaSW <- unique(lastnaImenaSW)
  close(conn)
  corpus <- tm_map(corpus, removeWords, lastnaImenaSW)
  
  # Predobdelava kopusa
  corpus  <- tm_map(corpus , removeNumbers)
  corpus  <- tm_map(corpus , removePunctuation)
  #Za kake posebne znake lahko uporabimo
  #removeSpecialChars <- function(x) gsub("“•”","",x)
  #corpus <- tm_map(corpus, removeSpecialChars)
  corpus  <- tm_map(corpus , content_transformer(tolower))
  corpus  <- tm_map(corpus , removeWords, stopwords('english'))
  corpus  <- tm_map(corpus , stemDocument)
  corpus  <- tm_map(corpus , stripWhitespace)
  #TODO odstrani lastna imena!
  corpus
}

preurediDatasetOdlomki <- function(corpus){
  dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))
  tdm <- TermDocumentMatrix(corpus);
  #dtm <- removeSparseTerms(dtm, 0.1);
  tdm <- removeSparseTerms(tdm, 0.7);
  
  dtm_matrix <- as.matrix(dtm);
  tdm_matrix <- as.matrix(tdm);
  #print(ncol(dtm_matrix));
  
  vector_unique <- vector();
  vector_common <- vector();
  vector_class <- vector();
  
  ##Topic - ciljna spremenljivka (zadnji stolpec matrike)
  topic_df <- read.csv("textiFiction.csv");
  topic_v <- topic_df[[5]];
  topic_v <- rep(topic_v, each=20);
  topic_v <- as.vector(topic_v);
  

  ##Unique - dodan atribut1
  for(row in 1:nrow(dtm_matrix)){
    i <- 0;
    for(col in 1:ncol(dtm_matrix)-1){
      polje <- dtm_matrix[row,col];
      if(!is.null(polje) &&
         length(polje) == 1 &&
         !is.na(polje) &&
         as.numeric(polje)!=0){
        i <- i + 1;
      }
      if(!is.numeric(polje)){print(polje);} #Debugging: ce polje ni numericno je nekaj narobe
    }
    vector_unique <- c(vector_unique, i);
  }
  
  ##Instanc pogostih besed - dodan atribut2
  vector_common = colSums(tdm_matrix);
  
  #print(ncol(dtm_matrix));
  
  dtm_matrix <- cbind(dtm_matrix, vector_unique);
  dtm_matrix <- cbind(dtm_matrix, vector_common);
  dtm_matrix <- cbind(dtm_matrix, topic_v);
  names(dtm_matrix)[ncol(dtm_matrix)] <- "Topic"
  return(dtm_matrix);
}

corpus <- narediKorpusOdlomkov();
dtm_m <- preurediDatasetOdlomki(corpus)
names(dtm_m)[ncol(dtm_m)] <- "Topic"

sel <- sample(nrow(dtm_m),nrow(dtm_m)*0.3,F);
train <- dtm_m[-sel,]
test <- dtm_m[sel,]

library(class)
##KNN
r <- which(names(dtm_m)=="Topic")
predicted <- knn(train[,-r], test[,-r], train[,ncol(train)], k=1)
observed <- test[,ncol(test)]
t <- table(observed, predicted)
t
sum(diag(t))/sum(t) #CA

library(kernlab)
#SVN
model.svm <- ksvm(Topic ~ ., train, kernel = "rbfdot")
predicted <- predict(model.svm, test, type = "response")
t <- table(observed, predicted)
t
