source("main.R")

removeCommonTerms <- function (x, pct) 
{
  stopifnot(inherits(x, c("DocumentTermMatrix", "TermDocumentMatrix")), 
            is.numeric(pct), pct > 0, pct < 1)
  m <- if (inherits(x, "DocumentTermMatrix")) 
    t(x)
  else x
  t <- table(m$i) < m$ncol * (pct)
  termIndex <- as.numeric(names(t[t]))
  if (inherits(x, "DocumentTermMatrix")) 
    x[, termIndex]
  else x[termIndex, ]
}

###Doda stolpce (št. različnih besed) (št. pogostih besed (ki so v vsaj 70% doc)) na konec vrstice
preurediDataset <- function(corpus){
  dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))
  #tdm <- TermDocumentMatrix(corpus);
  #dtm <- removeSparseTerms(dtm, 0.1);
  tdm <- removeSparseTerms(tdm, 0.7);
  
  dtm_matrix <- as.matrix(dtm);
  tdm_matrix <- as.matrix(tdm);
  
  vector_unique <- vector();
  vector_common <- vector();
  vector_class <- vector();
  
  ##Topic - ciljna spremenljivka (zadnji stolpec matrike)
  topic_df <- as.data.frame(read.csv("textiFiction.csv")[ ,5]);
  for(each in topic_df){
    vector_class <- c(vector_class,each);
  }
  print(vector_class);
  ##unique - dodan atribut1
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
  
  ##instanc pogostih besed - dodan atribut2
  vector_common = colSums(tdm_matrix);

  dtm_matrix <- cbind(dtm_matrix, vector_unique);
  dtm_matrix <- cbind(dtm_matrix, vector_common);
  dtm_matrix <- cbind(dtm_matrix, vector_class);
  names(dtm_matrix)[ncol(dtm_matrix)] <- "Topic"
  return(dtm_matrix);
}

preurediDatasetOdlomki <- function(corpus){
  dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))
  tdm <- TermDocumentMatrix(corpus);
  dtm <- removeSparseTerms(dtm, 0.1);
  tdm <- removeSparseTerms(tdm, 0.7);
  
  dtm_matrix <- as.matrix(dtm);
  tdm_matrix <- as.matrix(tdm);
  
  vector_unique <- vector();
  vector_common <- vector();
  vector_class <- vector();
  
  ##Topic - ciljna spremenljivka (zadnji stolpec matrike)
  topic_df <- as.data.frame(read.csv("textiFiction.csv")[ ,5]);
  for(each in topic_df){
    i <- 1;
    while(i < 21){
      vector_class <- c(vector_class, each);
      i <- i + 1;
    }
  }
  #print(vector_class);
  print(length(vector_class));
  #rep(vector_class, each=20);
  print(length(vector_class));
  
  ##unique - dodan atribut1
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
  
  ##instanc pogostih besed - dodan atribut2
  vector_common = colSums(tdm_matrix);
  
  print(nrow(dtm_matrix));
  
  dtm_matrix <- cbind(dtm_matrix, vector_unique);
  dtm_matrix <- cbind(dtm_matrix, vector_common);
  dtm_matrix <- cbind(dtm_matrix, vector_class);
  names(dtm_matrix)[ncol(dtm_matrix)] <- "Topic"
  return(dtm_matrix);
}

#tdm <- TermDocumentMatrix(corpus);
#mat_tdm <- as.matrix(tdm);
#mat_dtm <- as.matrix(data);

#tdm2 <- removeSparseTerms(tdm,0.7)
#matrix1 <- as.matrix(tdm2)


#idx <- which(rownames(tdm) == "collar")

#findFreqTerms(tdm, lowfreq=300);
#termFrequency <- rowSums(as.matrix(tdm))
#termFrequency <- subset(termFrequency, termFrequency < 2)

library(class)

corpus2 <- narediKorpusOdlomkov();
dtm_m <- preurediDatasetOdlomki(corpus2)
vdfffnames(dtm_m)[ncol(dtm_m)] <- "Topic"
sel <- sample(nrow(dtm_m),25,F);
train <- dtm_m[-sel,]
test <- dtm_m[sel,]

##KNN
r <- which(names(data)=="Topic")
predicted <- knn(train[,-r], test[,-r], train[,ncol(train)], k=7)
observed <- test[,ncol(test)]
t <- table(observed, predicted)
t
# KA
sum(diag(t))/sum(t)

# Priklic (recall) predstavlja delez pravilno klasificiranih pomembnih dokumentov med vsemi pomembnimi dokumenti v korpusu (v konkretnem primeru glede na razred "general")
t[1,1]/sum(t[1,])

# Preciznost (precision) predstavlja delez pravilno klasificiranih dokumentov, ki so bili klasificirani kot pomembni (v konkretnem primeru glede na razred "general") 
t[1,1]/sum(t[,1])

# SVM z radialno bazno jedrno funkcijo - še ne dela (Error: protect(): protection stack overflow)?
library(kernlab)
model.svm <- ksvm(Topic ~ ., train, kernel = "rbfdot")
predicted <- predict(model.svm, test, type = "response")
t <- table(observed, predicted)
t
