# Posterior adequacy sampler for designs with more than one stimulus per trial
posterior_adequacy_transfers <- function (data, 
                                          iterations, 
                                          posterior, 
                                          participant_id) {
  
  n_posterior_samples <- dim(posterior$posterior_samples$epsilon)[1]
  
  independent_trials <- data$trial_condition[data$id == participant_id]
  
  stimulus_id <- data$stimulus[data$id == participant_id]
  
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
  
  epsilon <- posterior$posterior_samples$epsilon[, participant_id]
  
  hidden_states <- posterior$posterior_samples$hidden_states[, , participant_id, ]
  
  y_hat <- matrix(NA, nrow = length(iterations), ncol = length(independent_trials))
  
  position <- 0
  
  for (i in iterations){
    position <- position + 1
    
    count <- 0
    for (t in independent_trials) {
      count <- count + 1
      
      y_hat[position, count] <- rbinom(n = 1, size = 1,
                                   prob = hidden_states[stimulus_id[t], t, i] * 
                                     (1 - epsilon[i]) + 
                                     (1 - hidden_states[stimulus_id[t], t, i]) * 
                                     epsilon[i])
    }
  }
  return(y_hat)
}
