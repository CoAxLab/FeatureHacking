loocv_nb <- function(data, id){
  
  # Generate accuracy output
  n_sub = dim(data)[1]
  acc = matrix(NaN,nrow=n_sub,ncol=1)
  
  for (s in 1:n_sub){
    
    # Get your training & test subjects
    test_sub = s
    train_subs = setdiff(1:n_sub,s)
    
    # Use a naive bayes classifier
    nb = naive_bayes(x=data[train_subs,], y=id[train_subs])
    test = predict(nb, newdata=t(data.frame(data[test_sub,])))
    
    # Save the accuracy of the fold
    acc[s]=(test==id[test_sub]) 
  }
  
  # Push the output 
  return(acc)
}