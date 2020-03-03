forward_stepwise_regionsearch_loocvnb <- function(data, id, region_indx){
  
  # Forward stepwise search on the leave one out cross validation (loocv)
  # from the naive bayes classifier
  
  # Before you loop
  regions = unique(region_indx) # Set of regions
  target_regions = vector()  # final output vector of regions
  final_acc = vector()    # final output of acc values
  isDone = 0               # termination flag
  k = 1                    # counter for model complexity
  
  # Loop until you get a termination flag
  while (!isDone){
    
    # New list for this fold
    acc_list = matrix(NaN,ncol=length(regions),nrow=1)
    
    # Search through remaining list items
    for (r in 1:length(regions)){
      search_list = sort(c(target_regions,regions[r]))
      
      # Only get the pairings in the search list
      x = data[,region_indx %in% search_list]
      # nb = naive_bayes(x=X, y=id)
      # test = predict(nb)
      # 
      acc_list[r] = mean(loocv_nb(x, id))
      #acc_list[r] = mean(test==id)
    }
    
    # Evaluate performance
    best_region = regions[acc_list == min(acc_list)]
    target_regions = c(target_regions, best_region)
    regions = setdiff(regions, best_region)
    final_acc = c(final_acc,acc_list[acc_list == min(acc_list)])
    
    # End if ACC begins to downtick or you've gone through all of the regions
    if (k > 1) {
      if (is_empty(regions) | final_acc[k-1]>final_acc[k]){
        isDone=1
      }
    }
    
    k = k+1
  }
  
  out = data.frame(target_regions[-1],final_acc[-1])
  return(out)
}