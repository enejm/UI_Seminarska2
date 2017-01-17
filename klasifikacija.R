#OCENE
CA <- function(prave, napovedane){
  t <- table(prave, napovedane)
  sum(diag(t)) / sum(t)
}

Sensitivity <- function(observed, predicted, pos.class){
  t <- table(observed, predicted)
  t[pos.class, pos.class] / sum(t[pos.class,])
}

# Funkcija za izracun specificnosti modela
Specificity <- function(observed, predicted, pos.class){
  t <- table(observed, predicted)
  
  # identify the negative class name
  neg.class <- which(row.names(t) != pos.class)
  
  t[neg.class, neg.class] / sum(t[neg.class,])
}


#KLASIFIKACIJA
klasifikacijaClass <- function(learn, test){
  df <- data.frame()
  
  observed <- test$Topic
  
  #Večinski klasifikator
  maj.class <- which.max(table(learn$Topic))
  table(learn$Topic)
  vecinski <- table(test$Topic)[maj.class]/nrow(test)
  
  #sen <- Sensitivity(observed, factor(rep("Science Fiction", nrow(test)), levels=c("Science Fiction","Adventure", "Fantasy")), "Science Fiction")
  #spec <- Specificity(observed, factor(rep("Science Fiction", nrow(test)), levels=c("Science Fiction","Adventure", "Fantasy")), "Science Fiction")
  prik <- t[maj.class, maj.class]/sum(t[maj.class,])
  # Preciznost (v konkretnem primeru glede na razred "Science fiction") 
  prec <- t[maj.class, maj.class]/sum(t[,maj.class])
  #df <- rbind(df, c(list("Kvotni k."), vecinski, prik, prec))
  df <- rbind(df, c(list("Kvotni k."),vecinski, prik, prec))
  
  colnames(df) <- c("IME", "CA", "Priklic", "Preciznost")
  df$IME<- as.character(df$IME)
  
  
  #KNN
  library(class)
  # Poiscemo stolpec, ki predstavlja razred
  r <- which(names(data)=="Topic")
  # Uporabimo model KNN za dolocitev tematike dokumentov v testni mnozici
  predicted <- knn(train[,-r], test[,-r], train$Topic)
  t <- table(observed, predicted)
  ca <- CA(observed, predicted)
  #observed <- factor(observed, levels=c("Science Fiction","Adventure", "Fantasy"))
  #predicted <- factor(predicted, levels=c("Science Fiction","Adventure", "Fantasy"))
  # Priklic (v konkretnem primeru glede na razred "Science Fiction")
  prik <- t[maj.class, maj.class]/sum(t[maj.class,])
  # Preciznost (v konkretnem primeru glede na razred "Science fiction") 
  prec <- t[maj.class, maj.class]/sum(t[,maj.class])
  df <- rbind(df, list("KNN", ca, prik, prec))
  
  
  library(kernlab)
  # SVM z radialno bazno jedrno funkcijo
  model.svm <- ksvm(Topic ~ ., train, kernel = "rbfdot")
  predicted <- predict(model.svm, test, type = "response")
  t <- table(observed, predicted)
  ca <- CA(observed, predicted)
  #observed <- factor(observed, levels=c("Science Fiction","Adventure", "Fantasy"))
  #predicted <- factor(predicted, levels=c("Science Fiction","Adventure", "Fantasy"))
  # Priklic (v konkretnem primeru glede na razred "Science Fiction")
  prik <- t[maj.class, maj.class]/sum(t[maj.class,])
  # Preciznost (v konkretnem primeru glede na razred "Science fiction") 
  prec <- t[maj.class, maj.class]/sum(t[,maj.class])
  df <- rbind(df, list("KNN", ca, prik, prec))
  
  df
}


bla <- function(){
  #
  # Klasifikacija dokumentov z modelom SVM
  #
  
  library(kernlab)
  # SVM z radialno bazno jedrno funkcijo
  model.svm <- ksvm(Topic ~ ., train, kernel = "rbfdot")
  predicted <- predict(model.svm, test, type = "response")
  t <- table(observed, predicted)
  t
  
  # Klasifikacijska tocnost
  sum(diag(t))/sum(t)
  
  # Priklic (v konkretnem primeru glede na razred "general")
  t[1,1]/sum(t[1,])
  
  # Preciznost (v konkretnem primeru glede na razred "general")
  t[1,1]/sum(t[,1])
  
  
  # SVM s polinomsko jedrno funkcijo
  model.svm <- ksvm(Topic ~ ., train, kernel = "poly", kpar=list(degree=2))
  predicted <- predict(model.svm, test, type = "response")
  t <- table(observed, predicted)
  t
  
  # Klasifikacijska tocnost
  sum(diag(t))/sum(t)
  
  # Priklic (v konkretnem primeru glede na razred "general")
  t[maj.class, maj.class]/sum(t[1,])
  
  # Preciznost (v konkretnem primeru glede na razred "general") 
  t[1,1]/sum(t[,1])
}











