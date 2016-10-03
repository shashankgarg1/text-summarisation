titlewords_based_scoring<-function(corpus, qwerty){
x<-as.String(c(meta(corpus[[qwerty]])$heading,meta(corpus[[qwerty]])$title))

vecSource <- VectorSource(x)
cp<- VCorpus(vecSource)
text<-as.String(cp[[1]])

tf <- TermDocumentMatrix(cp, control = list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, wordLengths = c(1, Inf)))
mat <- inspect(tf)
terms <- dimnames(mat)$Terms

original_document<-as.String(corpus[[qwerty]])
para_token_annotator<-Simple_Para_Token_Annotator(blankline_tokenizer)
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator<-Maxent_POS_Tag_Annotator("en")
a1 <- annotate(original_document, list(para_token_annotator,sent_token_annotator,word_token_annotator))
spans_of_words <- as.Span(a1[a1$type == "word"])
spans_of_sentences <- as.Span(a1[a1$type == "sentence"])
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
total_words<-length(spans_of_words)
for(i in 1 : length(spans_of_sentences)){
  dummy <- 0
  for(j in 1 : length(terms))
    if((gregexpr(terms[j], original_document[spans_of_sentences[i]], ignore.case = TRUE,fixed=TRUE)[[1L]] != - 1)){
      temp <- 0
      temp <- length(c(gregexpr(terms[j], original_document[spans_of_sentences[i]], ignore.case = TRUE,fixed=TRUE)[[1L]])) 
      dummy <- dummy + temp;
      #print(dummy)
    }
  scoring <- c(scoring, dummy/sent_len[i])    
  scoring
}
scoring}