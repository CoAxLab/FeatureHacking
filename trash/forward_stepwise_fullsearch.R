forward_stepwise_fullsearch <- function(data, id){
  
  # Before you loop
  feature_indx = 1:dim(data)[2]     # index of all features
  features = unique(feature_indx) # Set of features
  target_features = vector()      # final output vector of features
  final_aic = vector()            # final output of aic values
  isDone = 0               # termination flag
  k = 1                    # counter for model complexity
  
  # Loop until you get a termination flag
  while (!isDone){
    
    # New list for this fold
    aic_list = matrix(NaN,ncol=length(features),nrow=1)
    
    # Search through remaining list items
    for (f in 1:length(features)){
      search_list = sort(c(target_features,features[f]))
      
      x = matrix(NaN,nrow=dim(data)[1],ncol=length(search_list))
      
      # This should be more elegant but I am lazy
      for (s in 1:length(search_list)){
          x[,s] = data[,feature_indx==search_list[s]]
      }

      glm.fit = glm(id ~ x, family=binomial)
      aic_list[f] = glm.fit$aic 
    }
    
    # Evaluate performance
    best_feature = features[aic_list == min(aic_list)]
    target_features = c(target_features, best_feature)
    features = setdiff(features, best_feature)
    final_aic = c(final_aic,aic_list[aic_list == min(aic_list)])
    
    # End if AIC begins to uptick or you've gone through all of the wrods
    if (k > 1) {
      if (is_empty(features) | final_aic[k-1]<final_aic[k]){
        isDone=1
      }
    }
    
    k = k+1
  }
  
  out = data.frame(target_features[-1],final_aic[-1])
  return(out)
}