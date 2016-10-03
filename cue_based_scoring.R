##sentence scoring using cue dictionary created
cue_based_scoring <- function(corpus, qwerty){
print("cue")
	cue_list<-read.csv(file="C:/Users/Atul/Desktop/summ/cue.csv",sep=",")

##	corpus <- VCorpus(DirSource("C:/Users/shaina/Desktop/text summarization/dataset/docs"), readerControl = list(reader = readPlain))
##x<-corpus	##creating copy of orriginal data

## stemming  docs
##for(i in 1:length(corpus))
##{
##stemDocument(corpus[[i]],"en")
##}

	curr_document<-as.String(corpus[[qwerty]])	##all docs get concatenated

	para_token_annotator<-Simple_Para_Token_Annotator(blankline_tokenizer)
	sent_token_annotator <- Maxent_Sent_Token_Annotator()
	word_token_annotator <- Maxent_Word_Token_Annotator()
	a1 <- annotate(curr_document, list(para_token_annotator,sent_token_annotator,word_token_annotator))

##	meta(corpus[[1]], tag = "paragraph") <- a1[a1$type =a2= "paragraph"]
##	metsa(corpus[[1]], tag = "sentence") <- a1[a1$type == "sentence"]
##	meta(corpus[[1]], tag = "word") <- a1[a1$type == "word"]


cue_based_score <- function(curr_document, a1){
spans_of_sentences <- as.Span(a1[a1$type == "sentence"])

scoring <- c()

for(i in 1 : length(spans_of_sentences)){
dummy <- 0

for(j in 1 : nrow(cue_list)){
if(gregexpr(as.character(cue_list[j,1]), curr_document[spans_of_sentences[i]],  fixed = TRUE)[[1L]] != - 1){
dummy <- dummy + length(c(gregexpr(as.character(cue_list[j,1]), curr_document[spans_of_sentences[i]], fixed = TRUE)[[1L]]))*cue_list[j,2]

}
}



 scoring <- c(scoring, dummy);
}
scoring
}

scoring <- cue_based_score(curr_document= as.String(corpus[[qwerty]]), a1 = a1)



}