similarity_measure_between_sentences <- function(corpus, a1){
  OriginalDocument <- as.String(OriginalDocument)
  spans_of_sentences <- as.Span(a1[a1$type == "sentence"])
  spans_of_words <- as.Span(a1[a1$type == "word"])
  no_of_sentences <- length(spans_of_sentences)
  
  len <- c()
  for(i in 1 : no_of_sentences){
    cnt <- 0
    for(j in 1 : length(spans_of_words))
      if(gregexpr(original_document[spans_of_words[j]], original_document[spans_of_sentences[i]], ignore.case = TRUE, fixed = TRUE)[[1L]] != - 1)
        cnt <- cnt + length(c(gregexpr(original_document[spans_of_words[j]], original_document[spans_of_sentences[i]], ignore.case = TRUE, fixed = TRUE)[[1L]]))
    len <- c(len, cnt)
  }
        
  OriginalDocument <- removeWords(corpus[[1]], stopwords(kind = "en"))
  OriginalDocument <- as.String(OriginalDocument)
  mat <- matrix(data = 0, nrow = no_of_sentences, ncol = no_of_sentences)
  
  for(i in 1 : (no_of_sentences - 1)){
    for(j in (i + 1) : no_of_sentences){
      for(k in 1 : length(spans_of_words)){
        if(gregexpr(OriginalDocument[spans_of_words[k]], OriginalDocument[spans_of_sentences[i]], ignore.case = TRUE, fixed = TRUE)[[1L]] != - 1 && gregexpr(OriginalDocument[spans_of_words[k]], OriginalDocument[spans_of_sentences[j]], ignore.case = TRUE, fixed = TRUE)[[1L]] != - 1){
          mat[i, j] <- mat[i, j] + 1;
        }
      }
      mat[i, j] = 2 * mat[i, j] / (len[i] + len[j]);
    }
  }
  mat
}