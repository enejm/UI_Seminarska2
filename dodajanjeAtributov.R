

# Zgradimo matriko pojavitev dokument-beseda. Vrstice matrike ustrezajo dokumentom v korpusu, stolpci pa besedam.
# Vrednost elementa (i,j) opisuje pojavitev j-te besede v i-tem dokumentu (v tem primeru uporabimo Tf-Idf utezevanje)
mat <- as.matrix(data.tfidf )

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

for (txt in dir('textsModifiedNum/')){
  print(txt)
}

corpus

s <- "Steven Allan Spielberg is an American filmmaker and business magnate. Spielberg is consistently considered as one of the leading pioneers of the New Hollywood era, as well as being viewed as one of the most popular and influential filmmakers in the history of cinema."
s <- as.String(s)

# zaznavanje stavkov 
sent_ann <- Maxent_Sent_Token_Annotator()
sent_ann
a1 <- annotate(s, sent_ann)
a1

# izpisimo posamezne stavke
s[a1]

length(s[a1])


# zaznavanje besed
word_ann <- Maxent_Word_Token_Annotator()
word_ann
a2 <- annotate(s, word_ann, a1)
a2


tm_map(corpus, sent_ann)

#tekst <- 
corpus$"005.txt"

corpus[[3]]

data(corpus)
dataframe<-data.frame(text=unlist(sapply(corpus, `[`, "content")), stringsAsFactors=F)

dataframe[1,]


corpus <-Corpus(DirSource("./textsModifiedNum2"))

as.string(strwrap(corpus[[2]]))




