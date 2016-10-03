makesummary<-function(corpus, qwerty, dec_tree)
{
print("makesummary")
  ptm<-proc.time()
  
	##rm(corpus)
	rm(original_document)
	rm(para_token_annotator)
	rm(sent_token_annotator)
	rm(word_token_annotator)
	rm(a1)
	rm(spans_of_sentences)
	rm(lexical1)
	rm(parano1)
	rm(paraoffset1)
	rm(paraoffset1)
	rm(titlewords1)
	rm(propernoun1)
	rm(termfrequency1)
	rm(tf_idf1)
	rm(sentences1)
	
  
  
  original_document<-as.String(corpus[[qwerty]])
  para_token_annotator<-Simple_Para_Token_Annotator(blankline_tokenizer)
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a1 <- annotate(original_document, list(para_token_annotator,sent_token_annotator,word_token_annotator))
  a1 <- annotate(original_document, Maxent_POS_Tag_Annotator(), a1)
  
	meta(corpus[[qwerty]], tag = "paragraph") <- a1[a1$type == "paragraph"]
	meta(corpus[[qwerty]], tag = "sentence") <- a1[a1$type == "sentence"]
	meta(corpus[[qwerty]], tag = "word") <- a1[a1$type == "word"]
	spans_of_sentences <- as.Span(a1[a1$type == "sentence"])
	lexical1 <- lexical_based_scoring(corpus, qwerty)
	
	parano1<-parano_based_scoring(corpus, qwerty)
	
	paraoffset1<-paraoffset_based_scoring(corpus, qwerty)
	#titlewords1<-titlewords_based_scoring(corpus, qwerty)
	propernoun1<-propernoun_based_scoring(corpus, qwerty)
	termfrequency1<-termfrequency_based_scoring(corpus, qwerty)
	tf_idf1 <- tf_idf_based_scoring(corpus, qwerty)
	cue1 <- cue_based_scoring(corpus, qwerty)
	sentences1<-c()
   for(i in 1:length(spans_of_sentences))
    sentences1<-c(sentences1,original_document[spans_of_sentences[i]]) 
##print(length(lexical1))
##print(length(parano1))
##print(length(paraoffset1))
##print(length(propernoun1))
##print(length(termfrequency1))
##print(length(tf_idf1))
  df1<-data.frame(sentences=sentences1,parano=parano1,paraoffset=paraoffset1,propernoun=propernoun1,termfrequency=termfrequency1,lexical = lexical1, tf_idf = tf_idf1, cue = cue1)
  #return(df[,2])
  summary <- c()
  for(i in 1:length(spans_of_sentences))
    if((classifier(df1[i,],dec_tree))=="yes")
      summary<-c(summary,original_document[spans_of_sentences[i]])
    
time<-proc.time()-ptm
logFile<-"C:\\Users\\Atul\\Desktop\\Output\\logs.txt"
cat("\n User System Elapsed\n",file=logFile,append=TRUE)
cat( time,file=logFile,append=TRUE)
print(summary)
 summary   
 
}