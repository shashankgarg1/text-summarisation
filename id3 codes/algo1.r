algo1_heuristic<-function(attr_V,attr_C){

print("algo1_heuristic")
fac_C<-factor(attr_C)
values_of_C<-levels(fac_C)



fac_V<-factor(attr_V)
values_of_V<-levels(fac_V)



#names(df)<-c("ci","occ")

attr_disorder<-0
for(i in 1:length(values_of_V)){
	df<-data.frame(values_of_C,0)


	for(j in 1:length(attr_V)){
		if(attr_V[j]==values_of_V[i]){
		x<-attr_C[j]
		for(k in 1:length(df)){
			if(df[k,1]==x)
			{
			df[k,2]<-df[k,2]+1
			break
			}
		}

		}
	}
	#print(df)

	for(k in 1:length(df)){
		df[k,2]<-df[k,2]/length(attr_V)
	}					##occurence probability

	max_occ_prob<-0

	for(k in 1:length(df)){
		if(df[k,2]>max_occ_prob)
		max_occ_prob<-df[k,2]
	}

	#print(max_occ_prob)
	attr_disorder<-attr_disorder+max_occ_prob
	#print(attr_disorder)
}

attr_disorder<-attr_disorder/length(values_of_V)

attr_disorder
}