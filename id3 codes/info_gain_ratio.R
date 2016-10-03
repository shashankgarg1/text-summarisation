info_gain_ratio<-function(attr_V,attr_C){
	print("In Info_Gain_Ratio")
	fac_V<-factor(attr_V)
	no_occ_V<-table(fac_V)
	occ_prob_V<-table(fac_V)/length(attr_V)
	level_V <- levels(fac_V)
	
	fac_C<-factor(attr_C)
	occ_prob_C<-table(fac_C)/length(attr_C)
	level_C <- levels(fac_C)

	df<-data.frame(attr=attr_V,category=attr_C)
	cond_prob<-matrix(0,nrow=length(level_V),ncol=length(level_C))

	for(i in 1:length(level_V))
	{
		count_cat<-rep(0,length(level_C))
		
		for(j in 1:length(attr_V))
		{	
			if(level_V[i]==as.character(df[j,1]))
			{
				for(k in 1:length(level_C))
				{	if(df[j,2]==level_C[k])
					count_cat[k]<-count_cat[k]+1				
				}
			}
		}

		for(k in 1:length(level_C))
		{
			cond_prob[i,k]<-count_cat[k]/no_occ_V[i]
		}
	}
	#calculating entropy of info
	temp <- log(occ_prob_C)
	entropy_info <- occ_prob_C * temp
	entropy_info <- sum(entropy_info) * -1 / log(2)

	#conditional entropy
	temp<-log(cond_prob)
	temp<-temp*cond_prob
	
	row_sum <- c()
	row_sum <- apply(temp, 1, sum)

	temp<-row_sum*occ_prob_V
	
	cond_entropy <- sum(temp, na.rm = TRUE) * -1 / log(2)

 	
	#calcul info gain
	ltemp<-log(cond_prob)/log(2)

	prod_temp <- matrix(0, nrow = length(level_V), ncol = length(level_C))
	for(k in 1:length(level_C))
		prod_temp[,k]<-cond_prob[,k]*occ_prob_V
	temp<-prod_temp*ltemp*-1
	
	temp<-apply(temp,1,sum, na.rm=TRUE)
	temp<-sum(temp)

	info_gain<-entropy_info-temp

	#calc entropy of attribute
	temp <- log(occ_prob_V)
	entropy_info_attr <- occ_prob_V * temp
	entropy_info_attr <- sum(entropy_info_attr) * -1 / log(2)

	gain_ratio<-info_gain/entropy_info_attr

	gain_ratio
}