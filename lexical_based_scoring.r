lexical_based_scoring <- function(corpus, qwerty){
print("lexical")
len_word <- length(meta(corpus[[qwerty]])$word)
noun_words <- c()
for(i in 1 : len_word)
  if((meta(corpus[[qwerty]])$word$features[[i]] == "NN") || (meta(corpus[[qwerty]])$word$features[[i]] == "NNS"))
  {
    noun_words <- meta(corpus[[qwerty]])$word[i]
    break
  }
for(i in i : len_word)
  if((meta(corpus[[qwerty]])$word$features[[i]] == "NN") || (meta(corpus[[qwerty]])$word$features[[i]] == "NNS"))
    noun_words <- c(noun_words, meta(corpus[[qwerty]])$word[i])

sentence_span <- meta(corpus[[qwerty]])$sentence
spans_of_sentences <- as.Span(sentence_span)
spans_of_words <- as.Span(noun_words)
original_document <- as.String(corpus[[qwerty]])

lst <- list()
for(i in 1 : length(sentence_span))
{
##  print(lst)
  for(j in 1 : length(spans_of_words))
  {
    if((gregexpr(original_document[spans_of_words[j]], original_document[spans_of_sentences[i]], ignore.case = TRUE, fixed = TRUE)[[1L]] != - 1) && (spans_of_words[j]$start >= spans_of_sentences[i]$start) && (spans_of_words[j]$end <= spans_of_sentences[i]$end))
    { 
      filter <- getTermFilter("ExactMatchFilter", tolower(as.character(original_document[spans_of_words[j]])), FALSE)
      terms <- getIndexTerms("NOUN", 1, filter)
##      print(as.character(original_document[spans_of_words[j]]))
      if(! is.null(terms[[1]]))
      {
        synsets <- getSynsets(terms[[1]])
        len_s <- length(synsets)
        related <- c()
        for(i in 1 : len_s)
        {
          temp <- c()
          temp <- tryCatch(getRelatedSynsets(synsets[[i]], "@"), error = function(condition){return(NA)}, warning = function(condition){return(NA)}, finally = {related <- c(related, temp)})
##          temp <- tryCatch(getRelatedSynsets(synsets[[i]], "#"), error = function(condition){return(NA)}, warning = function(condition){return(NA)}, finally = {related <- c(related, temp)})
        }
        related_word <- sapply(related, getWord)
        related_word <- unlist(related_word)
        syn <- getSynonyms(terms[[1]])
        word <- c(syn, related_word)
        word <- c(word, as.character(original_document[spans_of_words[j]]))
        word <- unique(word)
        words <- readLines(system.file("stopwords", "english.dat", package = "tm"))
        word <- remove_stopwords(word, words, TRUE)
        word <- tolower(word)
        word <- tokenize(word)
        word <- stemDocument(word)
        word <- unique(word)
        word <- word[word != " "]
        word <- word[word != ""]
        
        k <- 1
        flag <- 0
        while(k <= length(lst))
        {
          l <- 1
          while(l <= length(word))
          {
            if(lst[[k]][1] == word[l])
            {
              flag <- 1
              lst[[k]] <- c(lst[[k]], as.character(original_document[spans_of_words[j]]))
              break
            }
            
            l <- l + 1
          }
          k <- k + 1
        }
        if(flag == 0)
        {
          l <- 1
          while(l <= length(word))
          {
            lu <- length(lst)
            lst[[lu + 1]] <- c(word[l], as.character(original_document[spans_of_words[j]]))
            l <- l + 1
          }
        }
		}
		}
	}
	}
	
	scoring <- c()
	for(i in 1 : length(lst))
	{
	temp <- tolower(lst[[i]])
	unique_occurrences <- length(unique(temp[2 : length(temp)]))
	tot_length <- (length(temp) - 1)
	scoring <- c(scoring, (tot_length * (1 - (unique_occurrences / tot_length))))
	}

	threshold <- mean(scoring) + 2 * sd(scoring)
	scoring_sentences <- c(rep(0, times = length(spans_of_sentences)))
	for(i in 1 : length(scoring))
	{
	if(scoring[i] > threshold)
	{
		for(j in 1 : length(spans_of_sentences))
		{
			unique_occurrences <- unique(lst[[i]][2 : length(lst[[i]])])
		for(k in 1 : length(unique_occurrences))
		{
			if(gregexpr(unique_occurrences[k], original_document[spans_of_sentences[j]], ignore.case = FALSE, fixed = TRUE)[[1L]] != - 1)
			{
			scoring_sentences[j] <- scoring_sentences[j] + 1
			}
		}
		}
	}
	}
	
	
	#scoring_sentences <- scoring_sentences - mean(scoring_sentences)
	if(max(scoring_sentences) != 0)
	scoring_sentences <- scoring_sentences / max(scoring_sentences)
	
	scoring_sentences
}