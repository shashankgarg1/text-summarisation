select_attr2<-function(dataset){
print("select_attr2")
	vect<-vector()	
	if(ncol(dataset) == 1)
		return (NULL)
		
	for(i in 1 : (ncol(dataset) - 1)){
		temp <- algo1_heuristic(dataset[, i], dataset[, ncol(dataset)])
		vect <- c(vect, temp)
	}
	
	pos<-1
	max<-vect[1]
	for(i in 1:(ncol(dataset)-1)){
		if(max<vect[i]){
			pos<-i
			max<-vect[i]
		}
	}
	names(dataset[pos])
}