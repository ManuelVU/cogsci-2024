# Samples from the one step ahead posterior predictive distribution 

posterior_predictive <- function (iterations, posterior, similarity, 
                                  trial, participant_id) {
  
  n_posterior_samples <- dim(posterior$posterior_samples$epsilon)[1]
  
  stimulus <- dim(similarity)[1]
  
  
  if (missing(iterations)) {
    iterations <- seq(1, n_posterior_samples)
  } 
  else if (iterations > n_posterior_samples){
    
    iterations <- sample(seq(1, n_posterior_samples), size = iterations,
                         replace = TRUE)    
  } 
  else {
    iterations <- sample(seq(1, n_posterior_samples), size = iterations,
                         replace = FALSE)
  }

  predicted_state <- matrix(data = NA, nrow = stimulus, 
                         ncol = length(iterations))
  
  predicted_response <- matrix(data = NA, nrow = stimulus, 
                               ncol = length(iterations))
  
  epsilon <- posterior$posterior_samples$epsilon[, participant_id]
  
  alpha <- posterior$posterior_samples$alpha[, participant_id]
  
  beta <- posterior$posterior_samples$beta[, participant_id]
  
  hidden_state_t1 <- posterior$posterior_samples$hidden_states[, trial, 
                                                               participant_id, ]
  
  # estimate the probability of the next state
  for (i in 1:length(iterations)) {
    for (ss in 1:stimulus) {
      
      similarity_to_others <- (similarity[ss, ])[-ss]
      
      states_rest <- hidden_state_t1[-ss, iterations[i]]
      
      relative_sim_others <- state_similarity(states_vec = states_rest,
                                              similarity_mat = similarity_to_others,
                                              method = "average")
      
      prob_stay_a <- logit(x = alpha[iterations[i]] - relative_sim_others)
      prob_stay_b <- logit(x = beta[iterations[i]] + relative_sim_others)
      
      predicted_state[ss, i] <- 
        rbinom(n = 1, size = 1, 
               prob = hidden_state_t1[ss, iterations[i]] * prob_stay_b +
                 (1 - hidden_state_t1[ss, iterations[i]]) * (1 - prob_stay_a))
      
      predicted_response[ss,i] <- 
        rbinom(n = 1, size = 1,
               prob = predicted_state[ss, i] * (1 - epsilon[iterations[i]]) + 
                 (1 - predicted_state[ss, i]) * epsilon[iterations[i]])
      
    }
  }
  output <- list()
  
  output$y_hat <- predicted_response
  output$x_hat <- predicted_state
  
  return(output)
}
