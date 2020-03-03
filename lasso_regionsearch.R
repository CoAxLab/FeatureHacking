lasso_regionsearch <- function(data, id, subset){
  
  train_set = subset==1
  test_set  = subset==2
  eval_set  = subset==3
  
  # Find the best lasso constraint
  model.tune = cv.glmnet(data[train_set,], id[train_set], family="binomial")
  
  # Return lambda
  model.fit = glmnet(data[test_set,], id[test_set], family="binomial", lambda=model.tune$lambda.min)
  

  return(model.fit)
}