# This function computes the transition probabilities of other chains. It is 
# used by the FFBS algorithm in order to calculate the large that defines the
# conditional filtered probabilities.

# Note: if we define self similarity as 0 then the function is easy to program 
# and relative similarity needs to be computed only once

transition_others <- function(state_now, state_after, similarity, current_id,
                              alpha, beta){
  
  n_others <- length(state_now)
  
  transition_prob <- c(1, 1)
  
  diag(similarity) <- rep(x = 0, times = dim(similarity)[1])
  
  state_all <- matrix(data = NA, nrow = n_others + 1, ncol = 2)
  
  state_all[-current_id, 1] <- state_now
  state_all[-current_id, 2] <- state_now
  
  state_all[current_id, 1:2] <- c(0,1)
  
  relative_similarity <-  cbind(
    state_similarity(states_vec = state_all[, 1], similarity_mat = similarity,
                    method = "average"),
    state_similarity(states_vec = state_all[, 2], similarity_mat = similarity,
                     method = "average"))
  
  for (i in 1:n_others) {
    if (state_now[i] == 0 & state_after[i] == 0) {
      
      transition_prob[1] <- transition_prob[1] *
        logit(x = alpha - relative_similarity[i, 1]) 
      
      transition_prob[2] <- transition_prob[2] *
        logit(x = alpha - relative_similarity[i, 2])
      
    }
    else if (state_now[i] == 0 & state_after[i] == 1) {
      
      transition_prob[1] <- transition_prob[1] * 
        (1 - logit(x = alpha - relative_similarity[i, 1]))
      
      transition_prob[2] <- transition_prob[2] *
        (1 - logit(x = alpha - relative_similarity[i, 2]))
      
    }
    else if (state_now[i] == 1 & state_after[i] == 0) {
      
      transition_prob[1] <- transition_prob[1] * 
        (1 - logit(beta + relative_similarity[i, 1]))
      
      transition_prob[2] <- transition_prob[2] * 
        (1 - logit(beta + relative_similarity[i, 2]))
      
    }
    else {
      
      transition_prob[1] <- transition_prob[1] *
        logit(beta + relative_similarity[i, 1])
      
      transition_prob[2] <- transition_prob[2] * 
        logit(beta + relative_similarity[i, 2])
      
    }
  }
  return(transition_prob)
}

