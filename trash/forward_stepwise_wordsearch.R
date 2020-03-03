forward_stepwise_wordsearch <- function(data, id, word_indx){
  
  # Before you loop
  words = unique(word_indx) # Set of words
  target_words = vector()  # final output vector of words
  final_aic = vector()    # final output of aic values
  isDone = 0               # termination flag
  k = 1                    # counter for model complexity
  
  # Loop until you get a termination flag
  while (!isDone){
    
    # New list for this fold
    aic_list = matrix(NaN,ncol=length(words),nrow=1)
    
    # Search through remaining list items
    for (w in 1:length(words)){
      search_list = sort(c(target_words,words[w]))
      
      # This should be more elegant but I am lazy
      x = matrix(NaN,nrow=dim(data)[1],ncol=length(search_list))
      for (s in 1:length(search_list)){
        x[,s] = rowMeans(data[,word_indx==search_list[s]],2)
      }
      
      glm.fit = glm(id ~ x, family=binomial)
      aic_list[w] = glm.fit$aic 
    }
    
    #Evaluate performance
    best_word = words[aic_list == min(aic_list)]
    target_words = c(target_words, best_word)
    words = setdiff(words, best_word)
    final_aic = c(final_aic,aic_list[aic_list == min(aic_list)])

    # End if AIC begins to uptick or you've gone through all of the wrods
    if (k > 1) {
      if (is_empty(words) | final_aic[k-1]<final_aic[k]){
        isDone=1
      }
    }
    
    k = k+1
  }
  
  out = data.frame(target_words,final_aic)
  return(out)
}