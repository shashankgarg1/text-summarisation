create_summary <- function(input_path) {
	##load("C:\\Users\\Atul\\Desktop\\summ\\qt.Rdata")
	load("C:\\Users\\Atul\\Desktop\\summ\\treeforinfo_gain.Rdata")
	source("C:\\Users\\Atul\\Desktop\\summ\\lexical_based_scoring.R")
	source("C:\\Users\\Atul\\Desktop\\summ\\parano_based_scoring.R")
	source("C:\\Users\\Atul\\Desktop\\summ\\paraoffset_based_scoring.R")
	#source("C:\\Users\\Atul\\Desktop\\summ\\titlewords_based_scoring.R")
	source("C:\\Users\\Atul\\Desktop\\summ\\propernoun_based_scoring.R")
	source("C:\\Users\\Atul\\Desktop\\summ\\termfrequency_based_scoring .R")
	source("C:\\Users\\Atul\\Desktop\\summ\\tf-idf_based_scoring.R")
	source("C:\\Users\\Atul\\Desktop\\summ\\cue_based_scoring.R")
	
	source("C:\\Users\\Atul\\Desktop\\summ\\makesummary_infogain.R")
	source("C:\\Users\\Atul\\Desktop\\summ\\classifier.R")

	corpus <-VCorpus(DirSource(input_path), readerControl = list(reader = readPlain))
	
	for(qwerty in 1 : length(corpus)){
		sumt <- makesummary1(corpus, qwerty, t)
		##sumq <- makesummary(corpus, qwerty, q)
		##sump <- makesummary(corpus, qwerty, p)
		
		if(sumt == NULL)
			sumt <- as.String("NO SUMMARY CREATED");
		strt <- as.String(sprintf("C:\\Users\\Atul\\Desktop\\summ\\summary_t\\%d.txt", qwerty))
		fileConn1<-file(strt)
		writeLines(sumt, fileConn1)
		close(fileConn1)
		
		##print(sumq)
		##strq <- as.String(sprintf("C:\\Users\\Atul\\Desktop\\summ\\summary_q\\%d.txt", qwerty))
		##fileConn<-file(strq)
		##writeLines(sumq, fileConn)
		##close(fileConn)
		
		##strp <- as.String(sprintf("C:\\Users\\Atul\\Desktop\\summ\\summary_p\\%d.txt", qwerty))
		##fileConn<-file(strp)
		##writeLines(sump, fileConn)
		##close(fileConn)
	}
}