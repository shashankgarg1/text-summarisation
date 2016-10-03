tf_idf_based_scoring <- function(corpus, qwerty){
print("tf_idf_based_scoring")
	curr_document<-as.String(corpus[[qwerty]])	

	para_token_annotator<-Simple_Para_Token_Annotator(blankline_tokenizer)
	sent_token_annotator <- Maxent_Sent_Token_Annotator()
	word_token_annotator <- Maxent_Word_Token_Annotator()
	a1 <- annotate(curr_document, list(para_token_annotator,sent_token_annotator,word_token_annotator))

	##meta(corpus[[1]], tag = "paragraph") <- a1[a1$type == "paragraph"]
	##meta(corpus[[1]], tag = "sentence") <- a1[a1$type == "sentence"]
	##meta(corpus[[1]], tag = "word") <- a1[a1$type == "word"]


	tf <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE, stemming = TRUE, removeNumbers = TRUE, wordLengths = c(3, Inf)))
	length(tf)

	tf<-weightTfIdf(tf, normalize = TRUE)
	mat <- inspect(tf)
	print("tf_idf_based_scoring")
	frequent_terms <- findFreqTerms(tf)

	sentence_scoring <- function(frequent_terms, mat, original_document, a1){
		spans_of_sentences <- as.Span(a1[a1$type == "sentence"])
		scoring <- c()
		for(i in 1 : length(spans_of_sentences)){
			dummy <- 0
			for(j in 1 : length(frequent_terms))
				if(gregexpr(frequent_terms[j], original_document[spans_of_sentences[i]], ignore.case = TRUE)[[1L]] == - 1){
					temp <- 0
					temp <- length(c(gregexpr(frequent_terms[j], original_document[spans_of_sentences[i]], ignore.case = TRUE)[[1L]])) * mat[j, 1]
					dummy <- dummy + temp;
				}
			scoring <- c(scoring, dummy)
		}

		scoring
	}

	scoring <- sentence_scoring(frequent_terms = frequent_terms, mat = mat, original_document = as.String(corpus[[qwerty]]), a1 = a1)
	scoring
}