#evaluation

corpus1<- VCorpus(DirSource("C:\\Users\\Atul\\Desktop\\info_gain system n model summ\\test syssumm files"), readerControl = list(reader = readPlain))
corpus2<- VCorpus(DirSource("C:/Users/Atul/Desktop/info_gain system n model summ/test standardsumm files"), readerControl = list(reader = readPlain))

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

print("FOR ALGO 2")
print("recall")
print(recall)
print("precision")
print(precision)
print("fscore")
print(f_score)
rm(corpus1)
rm(corpus2)