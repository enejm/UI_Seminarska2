

narediKorpus <- function(){
  corpus <-Corpus(DirSource("./textsModifiedNum/"))
  
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
  removeSpecialChars <- function(x) {PlainTextDocument(gsub("[^a-zA-Z ]","",x))}
  corpus <- tm_map(corpus, removeSpecialChars)
  corpus  <- tm_map(corpus , content_transformer(tolower))
  corpus  <- tm_map(corpus , removeWords, stopwords('english'))
  corpus  <- tm_map(corpus , stemDocument)
  corpus  <- tm_map(corpus , stripWhitespace)
  #TODO odstrani lastna imena!
  corpus
}


### za odstranjevanje imen

lastnaImenaEnDokument <- function(i){
  s <- as.String(strwrap(corpus[[i]]))
  sent_ann <- Maxent_Sent_Token_Annotator()
  word_ann <- Maxent_Word_Token_Annotator() 
  person_ann <- Maxent_Entity_Annotator(kind = "person")
  date_ann <- Maxent_Entity_Annotator(kind = "date")
  location_ann <- Maxent_Entity_Annotator(kind = "location")
  organization_ann <- Maxent_Entity_Annotator(kind = "organization")
  ann <- annotate(s, list(sent_ann, word_ann, person_ann, date_ann, location_ann, organization_ann))
  entities <- function(annots, kind) 
  {
    k <- sapply(annots$features, `[[`, "kind")
    s[annots[k == kind]]
  }
  a2s <- subset(ann, type == "sentence")
  write(length(s[a2s]), file = "stStavkov", append = TRUE, sep = "\n")
  l <- entities(ann, "person")
  l <- c(l,entities(ann, "location"))
  l <- c(l,entities(ann, "organization")) #Odstranimo tudi to?
  l
}

lastnaImenaVsiDokumenti <- function(){
  for(i in 1:105){
    rez <- unique(lastnaImenaEnDokument(i))
    x <- c(i, rez)
    write(x, file = "lastnaImenaStopwords", append = TRUE, sep = "\n")
    gc()
  }
}

#Traja zelo dolgo, rezultati so shranjeni v lastnaImenaStopwords
#lastnaImenaVsiDokumenti()


pridobiSteviloBesed <- function(i){
  corpus  <- tm_map(corpus , removePunctuation)
  for(i in 1:105){
    s <- as.String(strwrap(corpus[[i]]))
    sent_ann <- Maxent_Sent_Token_Annotator()
    a1 <- annotate(s, sent_ann)
    word_ann <- Maxent_Word_Token_Annotator()
    ann <- annotate(s, word_ann, a1)
    a2w <- subset(ann, type == "word")
    print(length(a2w))
    write(length(s[a2w]), file = "stBesed", append = TRUE, sep = "\n")
    gc()
  }
}

#pridobiSteviloBesed()




getStStavkovBesedSamostalnikiov <- function(){
  corp1 <-Corpus(DirSource("./textsModifiedNum/"))
  corp2 <-Corpus(DirSource("./textsModifiedNum/"))
  corp2 <-tm_map(corp2 , removePunctuation)
  sent_ann <- Maxent_Sent_Token_Annotator()
  word_ann <- Maxent_Word_Token_Annotator()
  pos_ann <- Maxent_POS_Tag_Annotator()
  df <- data.frame()
  
  for(odl in 1:(length(dir("textsModifiedNum/")))){
    print(odl)
    s1 <- as.String(strwrap(corp1[[odl]]))
    s2 <- as.String(strwrap(corp2[[odl]]))
    #Stavki
    a1 <- annotate(s1, sent_ann)
    stStavkov <- length(s1[a1])
    
    a2s <- annotate(s2, sent_ann)
    a2 <- annotate(s2, word_ann, a2s)
    s2[a2]
    stBesed <- length(s2[a2])
    
    # zaznavanje stavcnih clenov
    a3 <- annotate(s2, pos_ann, a2)
    a3w <- subset(a3, type == "word")
    tags <- vector()
    for (i in 1:length(a3w$features))
      tags <- c(tags, a3w$features[[i]]$POS)
    
    tokenPOS <- cbind(s2[a3w], tags)
    # izpis samostalnikov v obdelanem tekstu
    stSamostalnikov <- length(tokenPOS[substr(tokenPOS[,2], 1, 2) == "NN", 1])
    
    analiza <- c(stStavkov, stBesed, stSamostalnikov, stBesed/stStavkov, stSamostalnikov/stStavkov)
    #write.table(analiza, file = "analiza", append = TRUE, sep = " ")
    df <- rbind(df, analiza)
  }
  write.table(df, file = "analiza", sep = " ")
  df
}

#dfAnaliza <- getStStavkovBesedSamostalnikiov()
pripniDodatneAtribute <- function(data.tfidf){
  dfA <- as.data.frame(read.table("analiza"))
  data <- cbind(as.matrix(data.tfidf), dfA[4])
  data <- cbind(data, dfA[5])
  dfT <- as.data.frame(read.table("podTomaz"))
  data <- cbind(data, dfT[1])
  data <- cbind(data, dfT[2])
  data
}
