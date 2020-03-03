sim_cheat_exp_with_lasso <- function(p, n_iterations) {
  labels = matrix(c(rep('a',p.n_sub/p.n_group),rep('b',p.n_sub/p.n_group)),nrow=p.n_sub,ncol=1)  
  region_indx = rep(1:p.n_regions, each=p.n_voxels)
  subset_indx = rep(1:3,p.n_sub/3)
  bin_labels = matrix(c(rep(1,p.n_sub/p.n_group),rep(0,p.n_sub/p.n_group)),nrow=p.n_sub,ncol=1)
  
  # Setup output structures
  run_acc   = vector()
  cv_acc    = vector()
  test_acc  = vector()
  train_acc = vector()
  cheat_acc = vector()
  p_cheat_greater = vector()
  p_cv_greater = vector()
  p_test_greater = vector()
  
  for (i in 1:n_iterations) {
    # Generate new data st
    reg_resp = generate_data(p)
    run_acc[i] = mean(loocv_nb(reg_resp, labels))
    
    # break into two sets
    # Now make new test & training sets
    test_X = reg_resp[subset_indx==3,]
    test_Y = labels[subset_indx==3]
    train_X = reg_resp[subset_indx==2,]
    train_Y = labels[subset_indx==2]
    
    # Use forward stepwise search using LASSO to find best
    # features (regions) to use
    # Let's use LASSO to find the best regions
    model = lasso_regionsearch(reg_resp,bin_labels, subset_indx)
    best_regions = which(coef(model)!=0)
    # n_regions = length(best_regions)
    # if (n_regions < 2) {
    #   # Need a minimum of 2 for GNB
    #   n_regions = 2
    # }
    
    # Take the output from LASSO and test in LOOCV
    best_indx = region_indx %in% sort(best_regions)
    cv_acc[i] = mean(loocv_nb(test_X[,best_indx],test_Y))
    
    # Now just get the LOOCV training and test accuracies to have
    test_acc[i] = mean(loocv_nb(test_X, test_Y))
    train_acc[i] = mean(loocv_nb(train_X, train_Y))

    # The hold out test accuracy looks like this
    best_indx = region_indx %in% sort(best_regions)
    
    # Lets run a lot of permutations of different region lengths
    cheat = cheat_codes(test_X, test_Y, region_indx)
    n_regions = dim(cheat)[1]
    
    re_run_boot = permutation_run(n_regions, test_X, test_Y, region_indx)
    best_model = re_run_boot[re_run_boot$acc == max(re_run_boot$acc),]
    if (dim(best_model)[1]>1){
      best_model = best_model[1,]
    }
    cheat_acc[i] = best_model$acc
    
    runs = vector()
    for (b in 1:5) {
      boot_run = permutation_run(n_regions, test_X, test_Y, region_indx)
      runs = rbind(runs, boot_run)
    }

    # Probabilities of permuted accuracies being greater than
    # either the cheated, properly vetted, or simple test accuracies
    p_cheat_greater[i] = sum(runs$acc > cheat_acc[i])/dim(runs)[1]
    p_cv_greater[i] = sum(runs$acc > cv_acc[i])/dim(runs)[1]
    p_test_greater[i] = sum(runs$acc > test_acc[i])/dim(runs)[1]
        
  }
  
  out = data.frame(run_acc, cv_acc, test_acc, train_acc, cheat_acc, p_cv_greater, p_cheat_greater, p_test_greater)
  return(out)
}
