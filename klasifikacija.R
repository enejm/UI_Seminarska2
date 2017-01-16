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
  
  sen <- Sensitivity(observed, factor(rep("Science Fiction", nrow(test)), levels=c("Science Fiction","Adventure", "Fantasy")), "Science Fiction")
  spec <- Specificity(observed, factor(rep("Science Fiction", nrow(test)), levels=c("Science Fiction","Adventure", "Fantasy")), "Science Fiction")
  df <- rbind(df, c(list("Vecinski k."), vecinski, sen, spec))
  
  df
}

#
# Klasifikacija dokumentov z modelom KNN
#

library(class)

# Poiscemo stolpec, ki predstavlja razred
r <- which(names(data)=="Topic")
r

# Uporabimo model KNN za dolocitev tematike dokumentov v testni mnozici
predicted <- knn(train[,-r], test[,-r], train$Topic)
observed <- test$Topic
t <- table(observed, predicted)
t

# Klasifikacijska tocnost
sum(diag(t))/sum(t)

# Priklic (recall) predstavlja delez pravilno klasificiranih pomembnih dokumentov med vsemi pomembnimi dokumenti v korpusu (v konkretnem primeru glede na razred "general")
t[1,1]/sum(t[1,])

# Preciznost (precision) predstavlja delez pravilno klasificiranih dokumentov, ki so bili klasificirani kot pomembni (v konkretnem primeru glede na razred "general") 
t[1,1]/sum(t[,1])



#
# Klasifikacija dokumentov z modelom SVM
#

library(kernlab)
gc()
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
t[1,1]/sum(t[1,])

# Preciznost (v konkretnem primeru glede na razred "general") 
t[1,1]/sum(t[,1])














