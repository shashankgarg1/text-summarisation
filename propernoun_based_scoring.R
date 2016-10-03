propernoun_based_scoring<-function(corpus, qwerty){
print("propernoun_based_scoring")
OriginalDocument <- as.String(corpus[[qwerty]])
a1 <- annotate(OriginalDocument, Simple_Para_Token_Annotator(blankline_tokenizer))

a1 <- annotate(OriginalDocument, Maxent_Sent_Token_Annotator(), a1)
a1 <- annotate(OriginalDocument, Maxent_Word_Token_Annotator(), a1)
a1 <- annotate(OriginalDocument, Maxent_POS_Tag_Annotator(), a1)
##meta(corpus[[1]], tag = "paragraph") <- a1[a1$type == "paragraph"]
##meta(corpus[[1]], tag = "sentence") <- a1[a1$type == "sentence"]
##meta(corpus[[1]], tag = "word") <- a1[a1$type == "word"]

spans_of_sentences <- as.Span(a1[a1$type == "sentence"])
spans_of_words <- as.Span(a1[a1$type == "word"])
a2 <- a1[a1$type == "word"]
scoring <- c()
j<-1
sent_len<-c()
for(i in 1 : length(spans_of_sentences))
{
  dummy<-0
  while(spans_of_words[j]$end<spans_of_sentences[i]$end)
  { 
    dummy<-dummy+1
    j<-j+1
  }
  sent_len<-c(sent_len,dummy)
}
##print(sent_len)
total_words<-length(spans_of_words)
temp<-c()
for(i in 1:length(spans_of_words))
{
  if((a2[i]$features[[1]]$POS == "NNP") || (a2[i]$features[[1]]$POS == "NNPS"))
  {temp<-c(temp,OriginalDocument[spans_of_words[i]])
  ## print(OriginalDocument[spans_of_words[i]])  
  }
  
}
temp<-as.String(temp)
vecSource <- VectorSource(temp)
cp<- VCorpus(vecSource)
text<-as.String(cp[[1]])

tf <- TermDocumentMatrix(cp, control = list(removePunctuation = TRUE, stopwords = FALSE, stemming = FALSE, wordLengths = c(1, Inf)))
mat <- inspect(tf)
terms <- dimnames(mat)$Terms

for(i in 1 : length(spans_of_sentences)){
  dummy <- 0
  cnt <- 0
  for(j in 1 : length(terms))
  {
    if(gregexpr(terms[j], OriginalDocument[spans_of_sentences[i]], ignore.case = TRUE, fixed = TRUE)[[1L]] != - 1)
    {   
      temp <- length(c(terms[j], OriginalDocument[spans_of_sentences[i]], ignore.case = TRUE, fixed = TRUE)[[1L]])
      dummy <- dummy + temp;
    }
  }
  
  ##print(dummy)
  scoring <- c(scoring, dummy / sent_len[i])
}
scoring
}