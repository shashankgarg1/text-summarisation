heuristic_node_shaina <- function(dataset,check){
	print("heuristic_node_shaina")
	#if(!nrow(dataset))
	#	return (NULL);
		
	if(length(levels(factor(dataset[, ncol(dataset)]))) == 1)
		return (dataset[1, ncol(dataset)])
	
  i<-1
  ##removing col with same value in all rows
  while(1)
  {
   if(i>=ncol(dataset))
    break
	  else if(length(levels(factor(dataset[, i]))) == 1&&i!=ncol(dataset)){
	    dataset[, i] <- NULL
			check <- check[- i]
      i<-i-1                      ##verify, if this to be done or not
		}
    i<-i+1
  }
	if(ncol(dataset) == 1)
		return (dataset[1,ncol(dataset)])    ##return class having largest frequency instead
	
	lst <- list()
	list1 <- list()
	node <- select_attr2(dataset)

	lst[[node]] <- list()
	pos <- 0
	for(i in 1 : length(names(dataset)))
		if(names(dataset[i]) == node){
			pos <- i;
			break;
		}

	if(check[pos] == "d"){
		list1 <- split(dataset, dataset[node])
		for(i in 1 : length(list1))
			list1[[i]][[node]] <- NULL
		check <- check[- pos]
		vect <- levels(factor(dataset[, node]))
		for(i in 1 : length(vect))
			lst[[node]][[vect[i]]] <- list()
	}

	else{
		split_value <- splitting_for_continuous_attributes(dataset[, node], dataset[, ncol(dataset)])

		left <- capture.output(cat("<=", split_value))
		right <- capture.output(cat(">", split_value))
		lst[[node]][[left]] <- list()
		lst[[node]][[right]] <- list()
		
		list1[[1]] <- subset(dataset, dataset[, node] <= split_value)
		list1[[2]] <- subset(dataset, dataset[, node] > split_value)
	}
  
	if(length(list1)==0)
	  return(lst)
	
	while(nrow(list1[[1]])==0)
	{
	  list1[[1]]<-NULL
	}
	##fragment so that leaf nodes having null not formed by cleaning the list1 
  i<-1
	while(1)
	  {
      if(i>length(list1))
         break
	    else if(nrow(list1[[i]])==0)
	    {
	      list1[[i]]<-NULL
	      i<-(i-1)
	    }
      i<-i+1
	  }

	if(length(list1)==0)
	  return(lst)
	else if(length(list1)==1)
	{
	  list1[[1]][node]<-NULL
	  lst[[node]] <- heuristic_node_shaina(list1[[1]], check)
	}
  
 	###fragment end
  	for(i in 1 : length(list1))
		lst[[node]][[i]] <- heuristic_node_shaina(list1[[i]], check)
	
	lst
}
	