forward_stepwise_regionsearch <- function(data, id, region_indx){
  
  # Before you loop
  regions = unique(region_indx) # Set of regions
  target_regions = vector()  # final output vector of regions
  final_aic = vector()    # final output of aic values
  isDone = 0               # termination flag
  k = 1                    # counter for model complexity
  
  # Loop until you get a termination flag
  while (!isDone){
    
    # New list for this fold
    aic_list = matrix(NaN,ncol=length(regions),nrow=1)
    
    # Search through remaining list items
    for (r in 1:length(regions)){
      search_list = sort(c(target_regions,regions[r]))
      
      # This should be more elegant but I am lazy
      # x = matrix(NaN,nrow=dim(data)[1],ncol=length(search_list))
      # for (s in 1:length(search_list)){
      #   x[,s] = rowMeans(data[,region_indx==search_list[s]],2)
      # }
      
      x = data[,region_indx %in% search_list]
      
      # check to make sure if intercept is included
      glm.fit = glm(id ~ x, family=binomial)
      aic_list[r] = glm.fit$aic 
    }
    
    # Evaluate performance
    best_region = regions[aic_list == min(aic_list)]
    target_regions = c(target_regions, best_region)
    regions = setdiff(regions, best_region)
    final_aic = c(final_aic,aic_list[aic_list == min(aic_list)])
    
    # End if AIC begins to uptick or you've gone through all of the regions
    if (k > 1) {
      if (is_empty(regions) | final_aic[k-1]<final_aic[k]){
        isDone=1
      }
    }
    
    k = k+1
  }
  
  out = data.frame(target_regions[-1],final_aic[-1])
  return(out)
}