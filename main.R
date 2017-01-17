#install.packages(c("tm", "SnowballC", "wordcloud", "proxy", "kernlab", "NLP", "openNLP")) 
#install.packages("openNLPmodels.en", repos="http://datacube.wu.ac.at/", type="source")

# Nalozimo knjiznico za tekstovno rudarjenje v sistemu R
library(tm)
library(NLP)
library(openNLP)
library(openNLPmodels.en)

source("corpus.R")
source("odlomki.R")

corpus <- narediKorpus()

#podTomaz <- preurediDatasetOdlomkiShrani(corpus)
#dfT <- as.data.frame(podTomaz)
#dfT2 <- cbind(dfT, as.vector(podTomaz))

########################################
#  4. Preoblikovanje korpusa v ucno mnozico
########################################

#
# Zgradimo matriko pojavitev dokument-beseda
data.tfidf <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))
#Odstranimo redke besede? 
data.tfidf  <- removeSparseTerms(data.tfidf , sparse=0.8)

data <- pripniDodatneAtribute(data.tfidf)
data[ncol(data)]

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
sel <- sample(nrow(data), nrow(data)*0.3, F)
sel
train <- data[-sel,]
test <- data[sel,]

############################################
# 5. KLASIFIKACIJA
#############################################
source("klasifikacija.R")

rezultati2 <- klasifikacijaClass(train, test)


library(ipred)
mypredict <- function(object, newdata){predict(object, newdata, type = "class")}



library(CORElearn)

# Model bomo zgradili s pomocjo funkcije "CoreModel", ki potrebuje informacijo o tem, kateri tip modela naj zgradi.
# Funkcijo za gradnjo modela napisemo tako, da klicu funkcije "CoreModel" dodamo parameter za izbiro tipa modela.
mymodel.coremodel <- function(formula, data, target.model){CoreModel(formula, data, model=target.model)}

# Funkcijo za generiranje napovedi napisemo tako, da iz dobljenih napovedi modela obdrzimo samo oznake razredov.
# Ko model vrne zahtevane napovedi, ga ne potrebujemo vec - zato ga odstranimo iz pomnilnika.
mypredict.coremodel <- function(object, newdata) {pred <- predict(object, newdata)$class; destroyModels(object); pred}

# 10-kratno precno preverjanje 
res <- errorest(w.Topic~., data=data, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "bayes")
1-res$error



# metoda "izloci enega"
res <- errorest(insurance~., data=ins, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "tree", est.para=control.errorest(k = nrow(ins)))
1-res$error


library(kknn)

knn.model <- kknn(Topic~., train, test, k = 5)
predicted <- fitted(knn.model)
ae<-mae(observed, predicted)
rae<-rmae(observed, predicted, mean(learnReg$PTS_DIFF))
se <- mse(observed, predicted)
rse<-rmae(observed, predicted, mean(learnReg$PTS_DIFF))
df <- rbind(df, list("KNN", ae, rae, se, rse))


library(CORElearn)
obj <- CoreModel(w.Topic ~ .,  data=train, model="bayes")
predicted <- predict(obj, test, type="class")
observed <- test$w.Topic

t <- table(observed, predicted)
sum(diag(t)) / sum(t)


colnames(test$"next") = c("w.next")
test$w.next
test$w.next

r <- which(colnames(train)=="next")
r
colnames(train)[r] <- "w.nasl"
colnames(test)[r] <- "w.nasl"

test$"break"

colnames(train) <- paste0(c("w."), colnames(train))
colnames(test) <- paste0(c("w."), colnames(test))
colnames(data) <- paste0(c("w."), colnames(data))





