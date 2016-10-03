parano_based_scoring<-function(corpus, qwerty){
print("parano_based_scoring")
original_document<-as.String(corpus[[qwerty]])

para_token_annotator<-Simple_Para_Token_Annotator(blankline_tokenizer)
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a1 <- annotate(original_document, list(para_token_annotator,sent_token_annotator,word_token_annotator))
spans_of_sentences <- as.Span(a1[a1$type == "sentence"])
spans_of_paragraphs<- as.Span(a1[a1$type == "paragraph"])
parano <- c()
j<-1
for(i in 1 : length(spans_of_sentences)){
  dummy<-j
  if(spans_of_sentences[i]$end>=spans_of_paragraphs[j]$end)
  {     
    j<-j+1
  }
  parano <- c(parano, dummy)
  parano
}
parano
score<-c(rep(0,length(parano)))
no_of_sent<-length(spans_of_sentences)
k<-no_of_sent/2
for(i in 1:(no_of_sent/2))
{
  score[i]<-k/(no_of_sent/2)
  score[no_of_sent-i+1]<-k/(no_of_sent/2)
  k<-k-1
}
score}
