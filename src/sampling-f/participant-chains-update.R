# Function that updates all unobserved states chains for a single participant, 
# this function is used in order to facilitate the parallelisation of the 
# sampler

forward_backward_all <- function(states_current, responses, similarity, 
                                 n_states, total_trials, total_chains, 
                                 epsilon, gamma, alpha, beta){
    
  for(k in 1:total_chains){
    
    updated_chain <- forward_backward(update_stimulus_id = k,
                                      unobserved_states = states_current,
                                      responses = responses[k, ],
                                      similarity = similarity,
                                      n_states = n_states,
                                      total_trials = total_trials,
                                      response_error = epsilon,
                                      initial_probability = gamma,
                                      inertia_category_a = alpha,
                                      inertia_category_b = beta)
    
    states_current[k, ] <- updated_chain[[1]]
  }
  
  return(states_current)
  
}
