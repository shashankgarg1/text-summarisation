paraoffset_based_scoring<-function(corpus, qwerty){
print("paraoffset")
original_document<-as.String(corpus[[qwerty]])

para_token_annotator<-Simple_Para_Token_Annotator(blankline_tokenizer)
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a1 <- annotate(original_document, list(para_token_annotator,sent_token_annotator,word_token_annotator))
spans_of_sentences <- as.Span(a1[a1$type == "sentence"])
spans_of_paragraphs<- as.Span(a1[a1$type == "paragraph"])
paraoffset <- c()
j<-1
k<-0
for(i in 1 : length(spans_of_sentences)){
  
  # if((spans_of_sentences[i]$start>=spans_of_paragraphs[j]$start)&&(spans_of_sentences[i]$end<=spans_of_paragraphs[j]$end))
  #{
  
  offset<-k
  k<-k+1
  #}
  if(spans_of_sentences[i]$end>=spans_of_paragraphs[j]$end)
  {    
    
    k<-0
    j<-j+1
  }
  
  paraoffset <- c(paraoffset, offset)    
  
}
paraoffset
score<-c(rep(0,length(paraoffset)))
no_of_sent<-length(spans_of_sentences)

for(i in 1:no_of_sent)
{
  if(paraoffset[i]==0)
    k<-1
  else if(paraoffset[i]<5)
    k<-k-1/5
  else
    k<-0
  score[i]<-k
}
score}