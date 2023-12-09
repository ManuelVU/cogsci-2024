# Function that calculates the gradient of the log joint posterior of the 
# inertia parameters in the model.

gradient_inertia <- function(states,
                             alpha_tilde,
                             beta_tilde,
                             kappa_tilde,
                             alpha_prior,
                             beta_prior,
                             kappa_prior,
                             similarity, 
                             total_trials,
                             n_stimulus){
  
  gradient <- c(0, 0)
  
  e_alpha_tilde <- exp(alpha_tilde)
  e_beta_tilde <- exp(beta_tilde)
  e_kappa_tilde <- exp(kappa_tilde)
  
  for (s in 1:n_stimulus) {
    
    similarity_to_others <- (similarity[s, ])[-s]
    
    states_rest <- states[-s, ]
    
    transition_id_alpha <- 
      cbind((states[s, -total_trials] == 0 & 
               dplyr::lead(states[s, ])[-total_trials] == 0),
            -(states[s, -total_trials] == 0 & 
                dplyr::lead(states[s, ])[-total_trials] == 1))
    
    transition_id_beta <- 
      cbind((states[s, -total_trials] == 1 & 
                 dplyr::lead(states[s, ])[-total_trials] == 1),
            -(states[s, -total_trials] == 1 & 
               dplyr::lead(states[s, ])[-total_trials] == 0))
    
    transition_id_kappa <- cbind(transition_id_alpha, transition_id_beta)
    
    exponential_alpha <- matrix(data = NA, nrow = total_trials - 1, ncol = 2)
    
    exponential_beta <- matrix(data = NA, nrow = total_trials - 1, ncol = 2)
    
    exponential_kappa <- matrix(data = NA, nrow = total_trials - 1, ncol = 4)
    
    for (t in 2:total_trials) {
      
      relative_sim_others <- 
        state_similarity(states_vec = states_rest[, (t - 1)],
                         similarity_mat = similarity_to_others,
                         method = "average")
      
      logit_alpha <- logit(x = e_alpha_tilde -
                             e_kappa_tilde * relative_sim_others)
      
      exponential_alpha[(t - 1), 2] <- logit_alpha * e_alpha_tilde
      
      exponential_alpha[(t - 1), 1] <- exponential_alpha[(t - 1), 2] *
        exp(-e_alpha_tilde + e_kappa_tilde * relative_sim_others)
      
      logit_beta <- logit(x = e_beta_tilde +
                            e_kappa_tilde * relative_sim_others)
      
      exponential_beta[(t - 1), 2] <- logit_beta * e_beta_tilde
      
      exponential_beta[(t - 1), 1] <- exponential_beta[(t - 1), 2] *
        exp(- e_beta_tilde - e_kappa_tilde * relative_sim_others)
      
      exponential_kappa[(t - 1), 2] <- logit_alpha * e_kappa_tilde *
        (-relative_sim_others)
      
      exponential_kappa[(t - 1), 1] <- exponential_kappa[(t - 1), 2] *
        exp(-e_alpha_tilde + e_kappa_tilde * relative_sim_others)
      
      exponential_kappa[(t - 1), 4] <- logit_beta * e_kappa_tilde *
        relative_sim_others
      
      exponential_kappa[(t - 1), 3] <- exponential_kappa[(t - 1), 4] *
        exp(-e_beta_tilde - e_kappa_tilde * relative_sim_others)
      
    }
    
    gradient_alpha <- 
      sum(transition_id_alpha * exponential_alpha) + 
      alpha_prior[1] -
      alpha_prior[2] * e_alpha_tilde
    
    gradient_beta <- 
      sum(transition_id_beta * exponential_beta) + 
      beta_prior[1] -
      beta_prior[2] * e_beta_tilde
    
    gradient_kappa <- 
      sum(transition_id_kappa * exponential_kappa) + 
      beta_prior[1] -
      beta_prior[2] * e_kappa_tilde
    
    gradient <- gradient + c(gradient_alpha, gradient_beta, gradient_kappa)
    
  }
  return(gradient)
}
