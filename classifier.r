classifier<-function(feature_df,dectree)
{
print("classifier")
  while(length(dectree[[1]])>=2)
  {feature<-names(dectree)
   
  pos<-0
  for(i in 1 : length(names(feature_df)))
    if(names(feature_df[i]) == feature)
    {
      pos <- i;
      break;
    }
  split_value<-as.String(names(dectree[[1]])[1])
  split_value<-split_value[4,nchar(split_value)]
 
  if(feature_df[pos]<=split_value)
    dectree<-dectree[[1]][[1]]
  else
    dectree<-dectree[[1]][[2]]
  }
  dectree
}
