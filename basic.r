##Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_66')
library(NLP)
library(openNLP)
library(tm)
library(wordnet)
library(XML)
library(tau)
library(rpart)
library(e1071)

setDict("C:\\Program Files (x86)\\WordNet\\2.1\\dict")

source("C:\\Users\\Atul\\Desktop\\summ\\id3 codes\\info_gain_ratio.R")
source("C:\\Users\\Atul\\Desktop\\summ\\id3 codes\\splitting_for_continuous_attributes.R")
source("C:\\Users\\Atul\\Desktop\\summ\\id3 codes\\select_attr1.R")
source("C:\\Users\\Atul\\Desktop\\summ\\id3 codes\\modified_makenode.R")
source("C:\\Users\\Atul\\Desktop\\summ\\id3 codes\\heuristic_node.R")
source("C:\\Users\\Atul\\Desktop\\summ\\id3 codes\\heuristic.R")
source("C:\\Users\\Atul\\Desktop\\summ\\id3 codes\\heuristic_node_shaina.R")
source("C:\\Users\\Atul\\Desktop\\summ\\id3 codes\\algo1.R")	##same as info_gain_ratio
source("C:\\Users\\Atul\\Desktop\\summ\\id3 codes\\call_algo1.R")	##same as select_attr1 file containing select_attr function


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

##corpus <- VCorpus(DirSource(directory = "C:\\Users\\Atul\\Desktop\\test"), readerControl = list(reader = readXML(spec = list(heading = list("node", "/case/name"), content = list("node", "/case/sentences/sentence")), doc = PlainTextDocument())))

corpus <-VCorpus(DirSource("C:\\Users\\Atul\\Desktop\\summ\\Datasets"), readerControl = list(reader = readPlain))
corpus_summ <-VCorpus(DirSource("C:\\Users\\Atul\\Desktop\\summ\\summary"), readerControl = list(reader = readPlain))

df<-data.frame(sentences=character(),parano=double(),paraoffset=double(),propernoun=double(),termfrequency=double(),lexical = double(), tf_idf = double(), cue = double(), class=character())
for(qwerty in 1 : length(corpus)){
original_document<-as.String(corpus[[qwerty]])
summ_document<-as.String(corpus_summ[[qwerty]])
spans_of_sentences <- c()

	OriginalDocument <- as.String(corpus[[qwerty]])
	a1 <- annotate(OriginalDocument, Simple_Para_Token_Annotator(blankline_tokenizer))
	a1 <- annotate(OriginalDocument, Maxent_Sent_Token_Annotator(), a1)
	a1 <- annotate(OriginalDocument, Maxent_Word_Token_Annotator(), a1)
	a1 <- annotate(OriginalDocument, Maxent_POS_Tag_Annotator(), a1)
	meta(corpus[[qwerty]], tag = "paragraph") <- a1[a1$type == "paragraph"]
	meta(corpus[[qwerty]], tag = "sentence") <- a1[a1$type == "sentence"]
	meta(corpus[[qwerty]], tag = "word") <- a1[a1$type == "word"]
	rm(OriginalDocument)
	rm(a1)

spans_of_sentences <- as.Span(meta(corpus[[qwerty]])$sentence)

	OriginalDocument <- as.String(corpus_summ[[qwerty]])
	a1 <- annotate(OriginalDocument, Maxent_Sent_Token_Annotator())
	meta(corpus_summ[[qwerty]], tag = "sentence") <- a1[a1$type == "sentence"]
	rm(OriginalDocument)
	rm(a1)


lexical1 <- lexical_based_scoring(corpus, qwerty)
parano1<-parano_based_scoring(corpus, qwerty)
paraoffset1<-paraoffset_based_scoring(corpus, qwerty)
#titlewords1<-titlewords_based_scoring(corpus, qwerty)
propernoun1<-propernoun_based_scoring(corpus, qwerty)
termfrequency1<-termfrequency_based_scoring(corpus, qwerty)
tf_idf1 <- tf_idf_based_scoring(corpus, qwerty)
cue1 <- cue_based_scoring(corpus, qwerty)

sentences1<-c()
class1<-c()
for(i in 1:length(spans_of_sentences))
{
  dummy<-"no"
 sentences1<-c(sentences1,original_document[spans_of_sentences[i]]) 
 #for(j in 1:length(spans_of_summsent))
 #{
   if(gregexpr(as.String(original_document[spans_of_sentences[i]]), summ_document,fixed=TRUE,ignore.case = TRUE)[[1L]] != - 1)
   {
     dummy<-"yes"
   }
 #}
 class1<-c(class1,dummy)
}
##print(class1)
df1<-data.frame(sentences=sentences1,parano=parano1,paraoffset=paraoffset1,propernoun=propernoun1,termfrequency=termfrequency1,lexical = lexical1, tf_idf = tf_idf1, cue = cue1, class=class1)
df <- rbind(df, df1)

rm(original_document)
}
save(df, file = "C:\\Users\\Atul\\Desktop\\summ\\df.Rdata")
t <- make_node(df[-c(1)],c("c","c","c","c","c","c","c","d"), 0)
q <- heuristic_node(df[-c(1)],c("c","c","c","c","c","c","c","d"))
p <- heuristic_node_shaina(df[-c(1)],c("c","c","c","c","c","c","c","d"))
##fit <- rpart(class ~ parano + paraoffset + propernoun+ termfrequency + lexical + tf_idf, method = "class", data = df)
##plot(fit)
##text(fit)

save(q, t, p, file = "C:\\Users\\Atul\\Desktop\\summ\\qt.Rdata")
