# Function used to generate the initial value of hidden states of all chains 
# and participants.

initial_states <- function(n_chains,
                           n_trials,
                           n_participants,
                           similarity,
                           initial_state_probability,
                           inertia_category_a,
                           inertia_category_b,
                           similarity_weight,
                           total_trials) {

  output <- array(data = NA, dim = c(n_chains, total_trials, n_participants))
  
  diag(similarity) <- 0
  
  for (p in 1:n_participants){
   
    output[, 1, p] <- rbinom(n = n_chains, size = 1, 
                             prob = initial_state_probability[p])
   
    for (t in 2:n_trials[p]) {
      for (cc in 1:n_chains) {
        
        similarity_to_others <- similarity[cc, ]
        
        relative_sim_others <- 
          state_similarity(states_vec = output[, (t - 1), p],
                           similarity_mat = similarity_to_others, 
                           method = "average")
        
        prob_stay_a <- logit(x = inertia_category_a[p] - 
                               similarity_weight[p] * relative_sim_others)
        prob_stay_b <- logit(x = inertia_category_b[p] + 
                               similarity_weight[p] * relative_sim_others)
        
        output[cc, t, p] <- ifelse(test = output[cc, (t - 1), p] == 1, 
                                   yes = rbinom(n = 1, size = 1, 
                                                prob = prob_stay_b),
                                   no = rbinom(n = 1, size = 1, 
                                               prob = (1 - prob_stay_a)))
      }
    }
  }
  return(output)
}
