#evaluation

logFile<-"C:\\Users\\Atul\\Desktop\\log.txt"


print("Original C4.5 Algorithim")
corpus1<- VCorpus(DirSource("C:\\Users\\Atul\\Desktop\\summ\\summary_t"), readerControl = list(reader = readPlain))
corpus2<- VCorpus(DirSource("C:/Users/Atul/Desktop/summ/t summary"), readerControl = list(reader = readPlain))

recall<-0
precision<-0

for(i in 1: length(corpus1))
{
standard<-corpus1[[i]]
system<-corpus2[[i]]

standard<-as.String(standard)
system<-as.String(system)

sent_tokenizer<-Maxent_Sent_Token_Annotator()
st<-annotate(standard,sent_tokenizer)
sys<-annotate(system,sent_tokenizer)

spans_of_sentences <- as.Span(sys[sys$type == "sentence"])

no_standard<-length(st)
no_system<-length(sys)
count<-0

for(j in 1:length(spans_of_sentences))
{
if(grepl(system[spans_of_sentences[j]],standard,fixed=TRUE)==TRUE)
count<-count+1
##print(count)
}
recall<-recall+count/no_standard
##print(recall)
precision<-precision+count/no_system
##print(precision)

}

recall<-recall/length(corpus1)
precision<-precision/length(corpus1)
f_score<-2*precision*recall/(precision+recall)

cat("\nInfo_gain_tree\n",file=logFile,append=TRUE)
cat("\n Recall\n",file=logFile,append=TRUE)
cat( recall,file=logFile,append=TRUE)
cat("\n Precision\n",file=logFile,append=TRUE)
cat( precision,file=logFile,append=TRUE)
cat("\n Fscore\n",file=logFile,append=TRUE)
cat( f_score,file=logFile,append=TRUE)

rm(corpus1)
rm(corpus2)

print("Algorithim 1")
corpus1<- VCorpus(DirSource("C:\\Users\\Atul\\Desktop\\summ\\summary_p"), readerControl = list(reader = readPlain))
corpus2<- VCorpus(DirSource("C:/Users/Atul/Desktop/summ/t summary"), readerControl = list(reader = readPlain))

recall<-0
precision<-0

for(i in 1: length(corpus1))
{
standard<-corpus1[[i]]
system<-corpus2[[i]]

standard<-as.String(standard)
system<-as.String(system)

sent_tokenizer<-Maxent_Sent_Token_Annotator()
st<-annotate(standard,sent_tokenizer)
sys<-annotate(system,sent_tokenizer)

spans_of_sentences <- as.Span(sys[sys$type == "sentence"])

no_standard<-length(st)
no_system<-length(sys)
count<-0

for(j in 1:length(spans_of_sentences))
{
if(grepl(system[spans_of_sentences[j]],standard,fixed=TRUE)==TRUE)
count<-count+1
##print(count)
}
recall<-recall+count/no_standard
##print(recall)
precision<-precision+count/no_system
##print(precision)

}

recall<-recall/length(corpus1)
precision<-precision/length(corpus1)
f_score<-2*precision*recall/(precision+recall)

cat("\nAlgo1\n",file=logFile,append=TRUE)
cat("\n Recall\n",file=logFile,append=TRUE)
cat( recall,file=logFile,append=TRUE)
cat("\n Precision\n",file=logFile,append=TRUE)
cat( precision,file=logFile,append=TRUE)
cat("\n Fscore\n",file=logFile,append=TRUE)
cat( f_score,file=logFile,append=TRUE)



rm(corpus1)
rm(corpus2)



print("Algorithim 2")
corpus1<- VCorpus(DirSource("C:\\Users\\Atul\\Desktop\\summ\\summary_q"), readerControl = list(reader = readPlain))
corpus2<- VCorpus(DirSource("C:/Users/Atul/Desktop/summ/t summary"), readerControl = list(reader = readPlain))

recall<-0
precision<-0

for(i in 1: length(corpus1))
{
standard<-corpus1[[i]]
system<-corpus2[[i]]

standard<-as.String(standard)
system<-as.String(system)

sent_tokenizer<-Maxent_Sent_Token_Annotator()
st<-annotate(standard,sent_tokenizer)
sys<-annotate(system,sent_tokenizer)

spans_of_sentences <- as.Span(sys[sys$type == "sentence"])

no_standard<-length(st)
no_system<-length(sys)
count<-0

for(j in 1:length(spans_of_sentences))
{
if(grepl(system[spans_of_sentences[j]],standard,fixed=TRUE)==TRUE)
count<-count+1
##print(count)
}
recall<-recall+count/no_standard
##print(recall)
precision<-precision+count/no_system
##print(precision)

}

recall<-recall/length(corpus1)
precision<-precision/length(corpus1)
f_score<-2*precision*recall/(precision+recall)

cat("\nAlgo2\n",file=logFile,append=TRUE)
cat("\n Recall\n",file=logFile,append=TRUE)
cat( recall,file=logFile,append=TRUE)
cat("\n Precision\n",file=logFile,append=TRUE)
cat( precision,file=logFile,append=TRUE)
cat("\n Fscore\n",file=logFile,append=TRUE)
cat( f_score,file=logFile,append=TRUE)

rm(corpus1)
rm(corpus2)