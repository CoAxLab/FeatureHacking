permutation_run <- function(r, data, id, region_indx){
  
  regions = unique(region_indx)
  
  # Critical numbers
  n_iterations = 100
  n_regions = length(regions)
  
  # Output data objects
  regions_used = matrix(NaN,nrow=n_iterations, ncol=r)
  acc          = matrix(NaN,nrow=n_iterations, ncol=1)
  
  # Loop through random permutations
  for (i in 1:n_iterations){
    
    # New subsets of words & regions
    regions2use = sample(1:n_regions)[1:r]
    
    indx = region_indx %in% sort(regions2use)
    new_data = data[,indx]
      
    if (length(regions2use < 2)) {
      new_data = t(t(new_data));
    }
    
    acc[i]= mean(loocv_nb(new_data, id))
    regions_used[i,] = regions2use
  }
  
  out = data.frame(regions_used, acc)
  return(out)
  
}