generate_data <- function(p){
  
  # Keep data object
  #reg_resp = matrix(NaN,nrow=p.n_sub,ncol=p.n_voxels*p.n_regions)
  full_resp = vector()
  
for (r in 1:p.n_regions){
  reg_resp = matrix(NaN,nrow=p.n_sub,ncol=p.n_voxels)  
  
  for (s in 1:p.n_sub){
    if (s <= p.n_sub/p.n_group) {
        reg_resp[s,]= rnorm(p.n_voxels,mean=p.mu_a[r],sd=p.sigma_a[r])+rnorm(p.n_voxels,mean=0,sd=p.sigma_sample)  
      } else {
        if (r==p.n_regions) {
          r
        }
        
        reg_resp[s,]= rnorm(p.n_voxels,mean=p.mu_b[r],sd=p.sigma_b[r])+rnorm(p.n_voxels,mean=0,sd=p.sigma_sample)  
      }
    }
   
  # Concatenate the data vector 
  full_resp = cbind(full_resp, reg_resp)
  }
  
  return(full_resp)
  
}