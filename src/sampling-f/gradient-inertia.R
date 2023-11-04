# Function that calculates the gradient of the log joint posterior of the 
# inertia parameters in the model.

gradient_inertia <- function(states, alpha_tilde, beta_tilde, 
                             alpha_prior, beta_prior, similarity, 
                             total_trials, n_stimulus){
  
  gradient <- c(0, 0)
  
  e_alpha_tilde <- exp(alpha_tilde)
  e_beta_tilde <- exp(beta_tilde)
  
  for (s in 1:n_stimulus) {
    
    similarity_to_others <- (similarity[s, ])[-s]
    
    states_rest <- states[-s, ]
    
    transition_id_alpha <- 
      cbind((states[s, -total_trials] == 0 & 
               dplyr::lead(states[s, ])[-total_trials] == 0) +
              (states[s, -total_trials] == 0 & 
                 dplyr::lead(states[s, ])[-total_trials] == 1),
            -(states[s, -total_trials] == 0 & 
                dplyr::lead(states[s, ])[-total_trials] == 1))
    
    transition_id_beta <- 
      cbind((states[s, -total_trials] == 1 & 
               dplyr::lead(states[s, ])[-total_trials] == 0) +
              (states[s, -total_trials] == 1 & 
                 dplyr::lead(states[s, ])[-total_trials] == 1),
            -(states[s, -total_trials] == 1 & 
               dplyr::lead(states[s, ])[-total_trials] == 0))
    
    exponential_alpha <- c()
    
    exponential_beta <- c()
    
    for (t in 2:total_trials) {
      
      relative_sim_others <- 
        state_similarity(states_vec = states_rest[, (t - 1)],
                         similarity_mat = similarity_to_others,
                         method = "average")
      
      exponential_alpha[(t - 1)] <- exp(alpha_tilde + relative_sim_others) /
        (exp(relative_sim_others) + exp(exp(e_alpha_tilde)))
      
      exponential_beta[(t - 1)] <- exp(beta_tilde - relative_sim_others) /
        (exp(-relative_sim_others) + exp(e_beta_tilde))
    }
    
    gradient_alpha <- 
      sum(transition_id_alpha *
            cbind(exponential_alpha,
                  rep(x = e_alpha_tilde, 
                      times = total_trials - 1))) + 
      alpha_prior[1] -
      alpha_prior[2] * e_alpha_tilde
    
    gradient_beta <- 
      sum(transition_id_beta *
            cbind(exponential_beta,
                  rep(x = e_beta_tilde, 
                      times = total_trials - 1))) + 
      beta_prior[1] -
      beta_prior[2] * e_beta_tilde
    
    gradient <- gradient + c(gradient_alpha, gradient_beta)
    
  }
  return(gradient)
}

  
# Test

# a <- readr::read_csv(file = "data/stimulus-features/lee-navarro-features.csv")
# b <- distinctive_ln(stimulus_features = a)
# d <- featural_distance(distinctive_features = b)
# k <- similarity_ij(decay_rate = 1, decay_function = 1, dissimilarity = d)
# 
# gradient_inertia(alpha_tilde = 1, beta_tilde = 2,
#                  states = matrix(rbinom(n = 9 * 5, size = 1, prob = 0.5),
#                                  ncol = 5, nrow = 9),
#                  total_trials = 5, n_stimulus = 9, similarity = k,
#                  alpha_prior = c(0.1, 0.1), beta_prior = c(0.1, 0.1))