create_summary <- function(input_path) {
print("create_summary")
	load("C:\\Users\\Atul\\Desktop\\summ\\qt.Rdata")
	##load("C:\\Users\\Atul\\Desktop\\summ\\p.Rdata")
	source("C:\\Users\\Atul\\Desktop\\summ\\lexical_based_scoring.R")
	source("C:\\Users\\Atul\\Desktop\\summ\\parano_based_scoring.R")
	source("C:\\Users\\Atul\\Desktop\\summ\\paraoffset_based_scoring.R")
	#source("C:\\Users\\Atul\\Desktop\\summ\\titlewords_based_scoring.R")
	source("C:\\Users\\Atul\\Desktop\\summ\\propernoun_based_scoring.R")
	source("C:\\Users\\Atul\\Desktop\\summ\\termfrequency_based_scoring .R")
	source("C:\\Users\\Atul\\Desktop\\summ\\tf-idf_based_scoring.R")
	source("C:\\Users\\Atul\\Desktop\\summ\\cue_based_scoring.R")
	
	source("C:\\Users\\Atul\\Desktop\\summ\\makesummary.R")
	source("C:\\Users\\Atul\\Desktop\\summ\\classifier.R")

	corpus <-VCorpus(DirSource(input_path), readerControl = list(reader = readPlain))
	
	for(qwerty in 1 : length(corpus)){
		sumt <- makesummary(corpus, qwerty, t)
		sumq <- makesummary(corpus, qwerty, q)
		sump <- makesummary(corpus, qwerty, p)
		
		strt <- as.String(sprintf("C:\\Users\\Atul\\Desktop\\summ\\summary_t\\%d.txt", qwerty))
		fileConn<-file(strt)
		writeLines(sumt, fileConn)
		close(fileConn)
		
		strq <- as.String(sprintf("C:\\Users\\Atul\\Desktop\\summ\\summary_q\\%d.txt", qwerty))
		fileConn<-file(strq)
		writeLines(sumq, fileConn)
		close(fileConn)
		
		strp <- as.String(sprintf("C:\\Users\\Atul\\Desktop\\summ\\summary_p\\%d.txt", qwerty))
		fileConn<-file(strp)
		writeLines(sump, fileConn)
		close(fileConn)
	}
}