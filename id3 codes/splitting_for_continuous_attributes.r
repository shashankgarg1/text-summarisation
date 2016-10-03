splitting_for_continuous_attributes <- function(node, attr_C){
print("splitting_for_continuous_attributes")
	dataset <- data.frame(node, attr_C)
	dataset <- dataset[order(dataset$node), ]
	
	if(length(node) == 1)
		return (node[1])
	#finding the threshold value
	threshold_vector <- c()
	for(i in 1 : (length(node) - 1)){
		if(dataset[i, ncol(dataset)] != dataset[i + 1, ncol(dataset)]){
			temp <- dataset[i, 1] + dataset[i + 1, 1]
			temp <- temp / 2
			threshold_vector <- c(threshold_vector,temp)
		}
	}
	
#	print(threshold_vector)
	entropy_value <- c()
	
	for(i in 1 : length(threshold_vector)){
		temp_dataset_left <- data.frame()
		temp_dataset_right <- data.frame()
		
		temp_dataset_left <- subset(dataset, dataset[, 1] <= threshold_vector[i])
		temp_dataset_right <- subset(dataset, dataset[, 1] > threshold_vector[i])
		
		#calculating entropy for left table
		fac_C<-factor(temp_dataset_left[, 2])
		occ_prob_C<-table(fac_C)/length(temp_dataset_left[, 2])
		level_C <- levels(fac_C)
		temp <- log(occ_prob_C)
		entropy_info_left <- occ_prob_C * temp
		entropy_info_left <- sum(entropy_info_left) * (- 1) / log(2)
		
		#calculating entropy for right table
		fac_C<-factor(temp_dataset_right[, 2])
		occ_prob_C<-table(fac_C)/length(temp_dataset_right[, 2])
		level_C <- levels(fac_C)
		temp <- log(occ_prob_C)
		entropy_info_right <- occ_prob_C * temp
		entropy_info_right <- sum(entropy_info_right) * (- 1) / log(2)
		
		store <- ((length(temp_dataset_left[, 1])  * entropy_info_left) + (length(temp_dataset_right[, 1]) * entropy_info_right)) / length(dataset[, 1])
		
		entropy_value <- c(entropy_value, store)
	}

	max <- entropy_value[1]
	pos <- 1
	for(i in 1 : length(entropy_value)){
		if(max < entropy_value[i]){
			max <- entropy_value[i]
			pos <- i
		}
	}
	
	threshold_value <- dataset[pos, 1] + dataset[pos + 1, 1]
	threshold_value <- threshold_value / 2
	threshold_value
}