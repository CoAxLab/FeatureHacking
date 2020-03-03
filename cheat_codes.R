cheat_codes <- function(data, id, region_indx){
  
  regions = unique(region_indx)
  final_acc = vector()    # final output of acc values
  num_regions = vector()
  isDone = 0               # termination flag
  k = 1                    # counter for model complexity
  
  # Loop until you get a termination flag
  while (!isDone){
    
    # Brute force the run
    runs = permutation_run(k+1, data, id, region_indx)
    
    # find the best accuracy
    final_acc[k] = max(runs$acc)
    num_regions[k] = k+1

    # save the output
    if (k>1) {
      if (k==length(regions) | final_acc[k-1]>final_acc[k]){
        isDone = 1
        best_set = runs[runs[,2]==final_acc[k],]
      }
    }
    k=k+1
  }

  best_run = runs
  out = data.frame(final_acc[1:k-1], num_regions[1:k-1])
  return(out)
  
}