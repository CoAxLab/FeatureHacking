rm(list=ls())

library(plot.matrix)
library(naivebayes)
library(boot)
library(purrr)
library("RColorBrewer")
source("generate_data.R")
source("loocv_nb.R")
source("forward_stepwise_regionsearch.R")
source("permutation_run.R")
source("sim_cheat_exp_with_lasso.R")
source("cheat_codes.R")
source("lasso_regionsearch.R")

# Sample sizes
p.n_sub = 34
p.n_group = 2
p.n_voxels = 1
p.n_regions = 30

# sampling parameters (all gaussian)
p.mu_a = runif(p.n_regions, min=-.7, max=.7) # mean of generative distirubtion for each region in group A
p.mu_b = runif(p.n_regions/2, min=-.7, max=.7) # mean of generative distirubtion for each region in group B
p.mu_b = c(p.mu_b, p.mu_a[(p.n_regions/2+1):p.n_regions]) # Let's make 1/2 of the regions come from different distributions
p.sigma_a = rep(1,p.n_regions)   # std of generative distirubtion for each region in group A
p.sigma_b = rep(1,p.n_regions)   # std of generative distirubtion for each region in group A
p.sigma_sample = 0.3 # Subject noise (assume it's equal across subjects)

# Generate data file
reg_resp = generate_data(p)

# Show the data for now
plot(reg_resp,col=brewer.pal(n=10,name="RdBu"))

# Make labels for groups
labels = matrix(c(rep('a',p.n_sub/p.n_group),rep('b',p.n_sub/p.n_group)),nrow=p.n_sub,ncol=1)

# Now let's loop through and see how well we do across iterations.
# this step takes a while
n_iterations = 100
acc = matrix(NaN,ncol=n_iterations,nrow=1)
for (i in 1:n_iterations){
  reg_resp = generate_data(p)
  
  run_acc = loocv_nb(reg_resp, labels)
  acc[i]=mean(run_acc)
}

hist(acc)

# Now let's just generate a normative example (repeat until decent accuracy for 
# demonstration purposes)
p.n_sub = 34*3
reg_resp = generate_data(p)
labels = matrix(c(rep('a',p.n_sub/p.n_group),rep('b',p.n_sub/p.n_group)),nrow=p.n_sub,ncol=1)
run_acc = mean(loocv_nb(reg_resp, labels))

# 3 groups: 1 to learn lambda, 1 to select regions/fit, 1 to test fit
subset_indx = rep(1:3,p.n_sub/p.n_group)


# Lists we are going to use to subset the data
region_indx = rep(1:p.n_regions, each=p.n_voxels)

# Let's take the data set that we have and split it
# into 2 samples. One we learn to find the best features on
# an done we test.
subset_indx = rep(1:3,p.n_sub/3)
training_subset = subset_indx %in% c(1,2)
bin_labels = matrix(c(rep(1,p.n_sub/p.n_group),rep(0,p.n_sub/p.n_group)),nrow=p.n_sub,ncol=1)

# Let's use LASSO to find the best regions
model = lasso_regionsearch(reg_resp,bin_labels, subset_indx)
best_regions = which(coef(model)!=0)

# Now make new test & training sets
test_X = reg_resp[subset_indx==3,]
test_Y = labels[subset_indx==3]
train_X = reg_resp[subset_indx==2,]
train_Y = labels[subset_indx==2]

# Take the output from LASSO and test in LOOCV
best_indx = region_indx %in% sort(best_regions)
correct_cv_acc = mean(loocv_nb(test_X[,best_indx],test_Y))

# Compare against test set accuracy without dim reduction
test_acc = mean(loocv_nb(test_X, test_Y))

# Now let's REALLY overfit. I mean just plain cheat
cheat = cheat_codes(test_X, test_Y, region_indx)
n_regions = dim(cheat)[1]-1
re_run_boot = permutation_run(n_regions, test_X, test_Y, region_indx)

# Take the best model and call it your best
best_model = re_run_boot[re_run_boot$acc == max(re_run_boot$acc),]
if (dim(best_model)[1]>1){
  best_model = best_model[1,]
}

cheat_acc = best_model$acc

# Now let's put it all in a function and run it a lot of times
perm_test = sim_cheat_exp_with_lasso(p, 5)