termfrequency_based_scoring<-function(corpus, qwerty){
print("termfrequency_based_scoring")
	OriginalDocument <- as.String(corpus[[qwerty]])
	a1 <- annotate(OriginalDocument, Simple_Para_Token_Annotator(blankline_tokenizer))
	a1 <- annotate(OriginalDocument, Maxent_Sent_Token_Annotator(), a1)
	a1 <- annotate(OriginalDocument, Maxent_Word_Token_Annotator(), a1)
	##meta(corpus[[1]], tag = "paragraph") <- a1[a1$type == "paragraph"]
	##meta(corpus[[1]], tag = "sentence") <- a1[a1$type == "sentence"]
	##meta(corpus[[1]], tag = "word") <- a1[a1$type == "word"]
	tf <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE, wordLengths = c(3, Inf)))
	length(tf)
	tf <- tf / sum(tf)
	mat <- inspect(tf)
	print("termfrequency_based_scoring")
	frequent_terms <- findFreqTerms(tf)
	spans_of_sentences <- as.Span(a1[a1$type == "sentence"])
	scoring <- c()
	for(i in 1 : length(spans_of_sentences)){
		dummy <- 0
		for(j in 1 : length(frequent_terms))
			if(gregexpr(frequent_terms[j], OriginalDocument[spans_of_sentences[i]], ignore.case = TRUE)[[1L]] != - 1){
				temp <- 0
				temp <- length(c(gregexpr(frequent_terms[j], OriginalDocument[spans_of_sentences[i]], ignore.case = TRUE)[[1L]])) * mat[j, 1]
				dummy <- dummy + temp;
			}
		scoring <- c(scoring, dummy)
	}
	scoring
}