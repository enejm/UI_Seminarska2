# Pred zacetkom poskrbite, da so v R namescene naslednje knjiznice:
#
#    install.packages(c("tm", "SnowballC", "wordcloud", "proxy", "kernlab", "NLP", "openNLP")) 


# potrebno je namestiti tudi knjiznico openNLPmodels.en 
#
#    install.packages("openNLPmodels.en", repos="http://datacube.wu.ac.at/", type="source")


# Nalozimo zbirko 425 clankov objevljenih v sestdesetih letih prejsnjega stoletja v reviji TIME
conn = file("time.mag.txt", open="r")
text = readLines(conn)
close(conn)

length(text)
head(text)


# Nalozimo knjiznico za tekstovno rudarjenje v sistemu R
library(tm)


# Zgradimo korpus na podlagi podanega vektorja stringov
corpus <- Corpus(VectorSource(text))


length(corpus)
content(corpus[[1]])


#
# Predobdelava besedil
#

# Vsa besedila spremenimo v male crke
corpus <- tm_map(corpus, content_transformer(tolower))

# Iz vseh besedil odstranimo locila
corpus <- tm_map(corpus, removePunctuation)

# Iz vseh besedil odstranimo stevilke
corpus <- tm_map(corpus, removeNumbers)

# Iz besedil odstranimo pogoste besede, ki ne vplivajo na temo in vsebino (kot so npr. vezniki, masila, ...)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

# Lahko uporabimo lasten seznam besed, ki jih zelimo odstraniti iz korpusa
conn = file("english.stop.txt", open="r")
mystopwords = readLines(conn)
close(conn)

# Izpisimo ta seznam besed
mystopwords

# Odstranimo te besede iz korpusa
corpus <- tm_map(corpus, removeWords, mystopwords)

# Izvedemo postopek krnjenja besed v vseh dokumentih korpusa.
# Krnjenje odreze koncnico besede tako, da ostane le njen koren 
# Na ta nacin zdruzimo pomensko sorodne, a razlicne besede v enotno obliko
corpus <- tm_map(corpus, stemDocument)

# Odstranimo odvecne presledke iz vseh besedil
corpus <- tm_map(corpus, stripWhitespace)

# Poglejmo vsebino prvega dokumenta v predelanem korpusu
content(corpus[[1]])



#
# Matrika pojavitev beseda-dokument
#

# Matrika pojavitev beseda-dokument opisuje pogostost pojavljanja besed v dokumentih.
# Vrstice matrike ustrezajo posameznim besedam, stolpci pa dokumentom v korpusu.
# Vrednost elementa (i,j) doloca pogostost pojavljanja i-te besede v j-tem dokumentu korpusa. 

tdm <- TermDocumentMatrix(corpus)

# V nasem primeru matrika pojavitev beseda-dokument vsebuje 14557 besed in 425 dokumentov. 
# Matrika ima zelo malo nenicelnih elementov (99% elementov so nicle)
tdm

# Izpisimo seznam besed v korpusu
rownames(tdm)

# Poiscimo besedo "moscow"
idx <- which(rownames(tdm) == "moscow")
idx

# Poglejmo pogostost pojavljanja besede "moscow" v dokumentih iz korpusa
inspect(tdm[idx,])



#
# Pogoste besede
#

# Poiscimo pogoste besede (v konkretnem primeru take, ki se pojavijo najmanj 300 krat v celotnem korpusu)
findFreqTerms(tdm, lowfreq=300)

# Ponazorimo frekvenco pojavitev najpogostejsih besed v korpusu
termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency >= 300)
barplot(termFrequency)

# Besedni oblak je nacin vizualizacije, pri katerem je seznam besed prikazan v obliki oblaka. 
# V konkretnem primeru, velikost in barva crk ponazarjata pogostost pojavitve besede v korpusu
library(wordcloud)

# Pogostost pojavljanja besed v korpusu dolocimo s sestevanjem elementov posameznih vrstic matrike pojavitev beseda-dokument
mat <- as.matrix(tdm)
wordFreq <- sort(rowSums(mat), decreasing=TRUE)
grayLevels <- gray((wordFreq+10) / (max(wordFreq)+10))
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=100, random.order=F, colors=grayLevels)

#
# Povezave
#

# Poiscimo besede, ki se v besedilih pogosto pojavljajo skupaj z besedo "moscow"
# (v konkretnem primeru zahtevamo koeficient korelacije nad 0.5)
findAssocs(tdm, "moscow", 0.5)



#
# Razvrscanje besed glede na skupno pojavitev v dokumentih 
#

# Odstranimo redke besede (v konkretnem primeru tiste, ki se ne pojavljajo vsaj v 70% dokumentov)
tdm2 <- removeSparseTerms(tdm, sparse=0.7)
mat <- as.matrix(tdm2)

# Razdalje med besedami lahko izracunamo z uporabo funkcije dist()
distMatrix <- dist(mat)

# S pomocjo hierarhicnega razvrscanja lahko identificiramo skupine besed, ki pogosto nastopajo skupaj v dokumentih
fit <- hclust(distMatrix, method="ward.D")

# Hierarhicno zgradbo slikovno ponazorimo z dendrogramom. Listi drevesa predstavljajo posamezne besede, 
# notranja vozlisca pa zdruzene podskupine besed
plot(fit)




#
# Razvrscanje dokumentov glede na skupno rabo besed
#

# Zgradimo matriko pojavitev dokument-beseda. Vrstice matrike ustrezajo dokumentom v korpusu, stolpci pa besedam.
# Vrednost elementa (i,j) opisuje pojavitev j-te besede v i-tem dokumentu (v tem primeru uporabimo Tf-Idf utezevanje)
dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))
mat <- as.matrix(dtm)

# Razvrscanje dokumentov z uporabo metode kmeans (v konkretnem primeru dokumente razvrscamo v 3 skupine) 
k <- 3
kmeansResult <- kmeans(mat, k)

# Izpisimo najpogostejse besede iz vsake skupine dokumentov.
# Ta izpis nam pomaga pri avtomatskem dolocanju teme dokumentov.
for (i in 1:k) 
{
	s <- sort(kmeansResult$centers[i,], decreasing=T)
	cat(names(s)[1:10], "\n")
}





#
# Podobnost med dokumenti
#

# Izracunajmo, koliko se dokumenti razlikujejo med seboj. Podobnost med dokumenti lahko izracunamo s pomocjo kosinusne razdalje.
# Dokumente predstavimo kot vektorje: besede predstavljajo dimenzije vektorskega prostora, frekvence besed v dokumentu pa 
# razseznosti v teh dimenzijah. Podobnost med dokumenti izrazimo kot razdaljo v vektorskem prostoru. Kosinusna razdalja oceni
# podobnost med dokumenti kot kosinus kota med vektorskimi predstavitvami dokumentov.
library(proxy)

# Izracunajmo razdaljo med prvim in drugim dokumentom v korpusu
content(corpus[[1]])
content(corpus[[2]])
dist(as.matrix(dtm)[c(1,2),], method = "cosine")

# Poiscimo najbolj podobna dokumenta v korpusu...
dtm2 <- removeSparseTerms(dtm, sparse=0.8)
mat <- as.matrix(dtm2)
dist.mat <- as.matrix(dist(mat, method = "cosine"))
sim.idx <- which(dist.mat == min(dist.mat[dist.mat > 0]), arr.ind = TRUE)
sim.idx

# Poglejmo najbolj podobna dokumenta
content(corpus[[sim.idx[1,1]]])
content(corpus[[sim.idx[1,2]]])
dist(mat[c(sim.idx[1,1], sim.idx[1,2]),], method = "cosine")


#
#
# Klasifikacija besedil
#
#

# Zgradimo korpus iz vseh dokumentov znotraj podanega direktorija
corpus <-Corpus(DirSource("economy"))

length(corpus )


# Predobdelava kopusa
corpus  <- tm_map(corpus , removeNumbers)
corpus  <- tm_map(corpus , removePunctuation)
corpus  <- tm_map(corpus , content_transformer(tolower))
corpus  <- tm_map(corpus , removeWords, stopwords())
corpus  <- tm_map(corpus , stemDocument)
corpus  <- tm_map(corpus , stripWhitespace)


#
# Preoblikovanje korpusa v ucno mnozico
#

# Zgradimo matriko pojavitev dokument-beseda
data.tfidf <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))

# Preberemo podatke o tematiki dokumentov, ki sluzijo kot razredi v klasifikaciji
Topic <-read.table("economy-topics.txt")

# Zdradimo ucno mnozico tako, da matriki pojavitev dokument-beseda dodamo stolpec s tematiko dokumentov (razred)
data <- cbind(as.matrix(data.tfidf), Topic)
names(data)[ncol(data)] <- "Topic"

# Ucno mnozico razdelimo na dejansko ucno mnozico in testno mnozico
sel <- sample(nrow(data), 200, F)
train <- data[-sel,]
test <- data[sel,]


#
# Klasifikacija dokumentov z modelom KNN
#

library(class)

# Poiscemo stolpec, ki predstavlja razred
r <- which(names(data)=="Topic")

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



#
# Stavcna analiza
#

library(NLP)
library(openNLP)
#library(openNLPmodels.en)


s <- "Steven Allan Spielberg is an American filmmaker and business magnate. Spielberg is consistently considered as one of the leading pioneers of the New Hollywood era, as well as being viewed as one of the most popular and influential filmmakers in the history of cinema."
s <- as.String(s)

# zaznavanje stavkov 
sent_ann <- Maxent_Sent_Token_Annotator()
sent_ann
a1 <- annotate(s, sent_ann)
a1

# izpisimo posamezne stavke
s[a1]


# zaznavanje besed
word_ann <- Maxent_Word_Token_Annotator()
word_ann
a2 <- annotate(s, word_ann, a1)
a2

# izpisimo posamezne besede
a2w <- subset(a2, type == "word")
s[a2w]


# zaznavanje stavcnih clenov
pos_ann <- Maxent_POS_Tag_Annotator()
pos_ann
a3 <- annotate(s, pos_ann, a2)
a3

# stavcni cleni, vloga besede v stavku
#
# NN - samostalnik, noun (boy)
# NNP - lastno ime, proper noun
# NNS - samostalnik mnozine, noun plural (boys)
# VB - glagol, verb (eat)
# VBD - verb past tense (ate)
# VBG - verb, gerund (eating)
# VBN - verb past participle (eaten)
# VBP - verb not 3sg pres (eat)
# VBZ - verb 3sg pres (eats) 
# DT - èlen, determiner/article (a, the)
# JJ - pridevnik, adjective (yellow, big, small)
# JJR - adjective comparative (bigger)
# JJS - adjective superlative (wildest)
# IN - predlog, preposition (of, in, by)
# PRP - zaimek, pronoun (he, they) 
# RB - prislov, adverb (quickly, never)
# RBR - adverb comparative (faster)
# RBS - adverb superlative (fastest) 
# CC - veznik, conjunction (and, but, or)
# CD - glavni stevnik, cardinal number
# ...
 

 
a3w <- subset(a3, type == "word")
a3w


tags <- vector()
for (i in 1:length(a3w$features))
	tags <- c(tags, a3w$features[[i]]$POS)

table(tags)


tokenPOS <- cbind(s[a3w], tags)
tokenPOS

# izpis samostalnikov v obdelanem tekstu
tokenPOS[substr(tokenPOS[,2], 1, 2) == "NN", 1]

# izpis glagolov v obdelanem tekstu
tokenPOS[substr(tokenPOS[,2], 1, 2) == "VB", 1]

# izpis pridevnikov v obdelanem tekstu
tokenPOS[substr(tokenPOS[,2], 1, 2) == "JJ", 1]


#
# Oznacevanje oseb in krajev
#

s <- "LeBron Raymone James (born on December 30, 1984, in Akron, Ohio) is an American professional basketball player for the Cleveland Cavaliers of the National Basketball Association (NBA)."
s <- as.String(s)

sent_ann <- Maxent_Sent_Token_Annotator()
word_ann <- Maxent_Word_Token_Annotator() 
person_ann <- Maxent_Entity_Annotator(kind = "person")
date_ann <- Maxent_Entity_Annotator(kind = "date")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")


ann <- annotate(s, list(sent_ann, word_ann, person_ann, date_ann, location_ann, organization_ann))
ann


entities <- function(annots, kind) 
{
	k <- sapply(annots$features, `[[`, "kind")
    	s[annots[k == kind]]
}

entities(ann, "person")
entities(ann, "date")
entities(ann, "location")
entities(ann, "organization")

