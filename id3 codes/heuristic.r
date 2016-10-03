heuristic <- function(dataset1){
print("heuristic")
	dataset <- dataset1
	val <- vector()
	for(j in 1 : (ncol(dataset) - 1))
		for(i in 1 : (nrow(dataset) - 1)){
		##print(i)
		print(j)
			if(as.numeric(dataset[i, j]) < 0.05){
				dataset[i, j] <- 2
				}
			else if(as.numeric(dataset[i, j]) < 0.1)
				dataset[i, j] <- 3
			else if(as.numeric(dataset[i, j]) < 0.15)
				dataset[i, j] <- 4
			else if(as.numeric(dataset[i, j]) < 0.2)
				dataset[i, j] <- 5
			else if(as.numeric(dataset[i, j]) < 0.25)
				dataset[i, j] <- 6
			else if(as.numeric(dataset[i, j]) < 0.30)
				dataset[i, j] <- 7
			else if(as.numeric(dataset[i, j]) < 0.35)
				dataset[i, j] <- 8
			else if(as.numeric(dataset[i, j]) < 0.4)
				dataset[i, j] <- 9
			else if(as.numeric(dataset[i, j]) < 0.45)
				dataset[i, j] <- 10
			else if(as.numeric(dataset[i, j]) < 0.50)
				dataset[i, j] <- 11
			else if(as.numeric(dataset[i, j]) < 0.55)
				dataset[i, j] <- 12
			else if(as.numeric(dataset[i, j]) < 0.60)
				dataset[i, j] <- 13
			else if(as.numeric(dataset[i, j]) < 0.65)
				dataset[i, j] <- 14
			else if(as.numeric(dataset[i, j]) < 0.70)
				dataset[i, j] <- 15
			else if(as.numeric(dataset[i, j]) < 0.75)
				dataset[i, j] <- 16
			else if(as.numeric(dataset[i, j]) < 0.8)
				dataset[i, j] <- 17
			else if(as.numeric(dataset[i, j]) < 0.85)
				dataset[i, j] <- 18
			else if(as.numeric(dataset[i, j]) < 0.90)
				dataset[i, j] <- 19
			else
				dataset[i, j] <- 20
		}
	for(i in 1 : (ncol(dataset) - 1)){
		dum <- 0
		tot_uniq <- length(unique(dataset[, i]))
		
		yes_sub <- subset(dataset, dataset[, ncol(dataset)] == "yes")
		no_sub <- subset(dataset, dataset[, ncol(dataset)] == "no")
		
		dec_yes1 <- length(unique(yes_sub[, i]))
		dec_no1 <- length(unique(no_sub[, i]))
		
		dum <- dum + (dec_yes1 / tot_uniq) + (dec_no1 / tot_uniq)
		
		for(j in 1 : (ncol(dataset) - 1))
			for(k in 1 : (ncol(dataset) - 1))
				if(j != i && k != i && j != k) {
					temp <- data.frame(col1 = dataset[, j], col2 = dataset[, k], col3 = dataset[, i], col4 = dataset[, ncol(dataset)])
					temp_tot_uniq <- length(unique((temp[, 1] ^ 2) + (temp[, 2] ^ 3) + (temp[, 3] ^ 4)))
					
					temp_yes_sub <- subset(temp, temp[, 4] == "yes")
					temp_no_sub <- subset(temp, temp[, 4] == "no")
					
					temp_dec_yes1 <- length(unique((temp_yes_sub[, 1] ^ 2) + (temp_yes_sub[, 2] ^ 3) + (temp_yes_sub[, 3] ^ 4)))
					
					temp_dec_no1 <- length(unique((temp_no_sub[, 1] ^ 2) + (temp_no_sub[, 2] ^ 3) + (temp_no_sub[, 3] ^ 4)))
					
					dum <- dum + (temp_dec_yes1 / temp_tot_uniq) + (temp_dec_no1 / temp_tot_uniq)
				}
		
		val <- c(val, dum)
	}
	
	pos<-1
	min<-val[1]
	##print(val)
	for(i in 1:(ncol(dataset)-1)){
		if(min>val[i]){
			pos<-i
			min<-val[i]
		}
	}
	names(dataset[pos])
}