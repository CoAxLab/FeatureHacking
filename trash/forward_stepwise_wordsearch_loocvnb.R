forward_stepwise_wordsearch_loocvnb <- function(data, id, word_indx){
  
  # Before you loop
  words = unique(word_indx) # Set of words
  target_words = vector()  # final output vector of words
  final_acc = vector()    # final output of acc values
  isDone = 0               # termination flag
  k = 1                    # counter for model complexity
  
  # Loop until you get a termination flag
  while (!isDone){
    
    # New list for this fold
    acc_list = matrix(NaN,ncol=length(words),nrow=1)
    
    # Search through remaining list items
    for (w in 1:length(words)){
      search_list = sort(c(target_words,words[w]))
      
      # Only get the pairings in the search list
      X = data[,word_indx %in% search_list]
      
      nb = naive_bayes(x=X, y=id)
      test = predict(nb)
      
      #acc_list[w] = mean(loocv_nb(x, id))
      acc_list[w] = mean(test==id)
    }
    
    #Evaluate performance
    best_word = words[acc_list == min(acc_list)]
    target_words = c(target_words, best_word)
    words = setdiff(words, best_word)
    final_acc = c(final_acc,acc_list[acc_list == min(acc_list)])

    # End if acc begins to uptick or you've gone through all of the wrods
    if (k > 1) {
      if (is_empty(words) | final_acc[k]==1) { #| final_acc[k-1]<final_acc[k] | ){
        isDone=1
      }
    }
    
    k = k+1
  }
  
  out = data.frame(target_words,final_acc)
  return(out)
}