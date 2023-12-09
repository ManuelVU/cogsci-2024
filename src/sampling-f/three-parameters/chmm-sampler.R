# CHMM of categorization sampling algorithm

chmm_sampling <- function(data_chmm, metric = "distinctive", order_p = 1, 
                          n_iterations, n_burn, n_cores,
                          parameters_initial_values,
                          start_step_size, hmc_acceptance, 
                          prior_parameters) {
  
  # Load library for parallel computing and register cores
  library(doParallel)
  library(foreach)
  if(n_cores > parallel::detectCores()){
    message("Number of cores selected is greater than available number.")
    
    message(paste(c("Setting number of cores to: ", parallel::detectCores()),
                  collapse = ""))
    
    n_cores <- parallel::detectCores()
  }
  
  doParallel::registerDoParallel(n_cores)
  
  # Separate data
  responses <- data_chmm$response
  trials_participant <- data_chmm$participant_t
  participants <- dim(responses)[3]
  stimulus <- dim(responses)[1]
  
  # Obtain similarity matrix from stimulus features
  if ("similarity" %in% names(data_chmm)) {
    
    stimulus_similarity <- data_chmm$similarity
    
  } else{
    
    features <- data_chmm$stimulus_features
    
    if (metric == "distinctive") {
      stimulus_distinctive <- distinctive_ln(stimulus_features = features)
      stimulus_distance <- featural_distance(distinctive_features = 
                                               stimulus_distinctive)    
    } else if (metric == "minkowski") {
      stimulus_distance <- minkowski_distance(stimulus_features = features,
                                              p = order_p)
    }
    
    stimulus_similarity <- similarity_ij(decay_rate = 1, decay_function = 1, 
                                         dissimilarity = stimulus_distance) 
  }
  
  # Start sample matrix for individual model parameters and add initial value 
  # to row one
  # initial probability parameter
  sample_gamma <- matrix(data = parameters_initial_values$gamma,
                         byrow = TRUE, 
                         ncol = participants, 
                         nrow = n_iterations)
  
  # trembling hand parameter
  sample_epsilon <- matrix(data = parameters_initial_values$epsilon, 
                           byrow = TRUE, 
                           ncol = participants, 
                           nrow = n_iterations)
  
  # stickines parameter for category 0
  sample_alpha <- matrix(data = parameters_initial_values$alpha, 
                         byrow = TRUE, 
                         ncol = participants, 
                         nrow = n_iterations)
  
  # stickines parameter for category 1
  sample_beta <- matrix(data = parameters_initial_values$beta, 
                        byrow = TRUE, 
                        ncol = participants, 
                        nrow = n_iterations)
  
  # weigth of similarity on probability to stay
  sample_kappa <- matrix(data = parameters_initial_values$kappa, 
                         byrow = TRUE, 
                         ncol = participants, 
                         nrow = n_iterations)
  
  # Add initial sample of states to initial values
  parameters_initial_values$states <- initial_states(
    n_chains = stimulus, 
    n_trials = trials_participant,
    n_participants = participants, 
    similarity = stimulus_similarity,
    initial_state_probability = sample_gamma[1, ],
    inertia_category_a = sample_alpha[1, ],
    inertia_category_b = sample_beta[1, ],
    similarity_weight = sample_kappa[1, ],
    total_trials = dim(responses)[2])
  
  # Start states array to store samples of states
  sample_states <- array(data = NA, dim = c(dim(responses), n_iterations))
  
  # Initialize states at their initial values
  sample_states[, , , 1] <- parameters_initial_values$states
  
  # Initialize step size and number of steps for HMC
  step_size <- start_step_size
  n_leaps <- 30
  acc <- matrix(data = NA, nrow = participants, ncol = 100)
  acc[,1] <- rep(x = 1, times = participants)
  
  # Start counter for acceptance rate
  count <- 1
  
  # Start progress bar
  progress <- txtProgressBar(min = 1, max = n_iterations, style = 3, 
                             width = 50, char = "=")
  
  # Start sampler
  for (sample in 2:n_iterations) {
    
    # Add to count position
    count <- count + 1
    
    # Update participants states in parallel
    st <- foreach(pp = 1:participants) %dopar% {
      forward_backward_all(states_current =
                             sample_states[, 1:trials_participant[pp], pp,
                                           (sample - 1)],
                           responses = responses[, 1:trials_participant[pp], pp],
                           similarity = stimulus_similarity,
                           n_states = 2,
                           total_trials = trials_participant[pp],
                           total_chains = stimulus,
                           epsilon = sample_epsilon[(sample - 1), pp],
                           gamma = sample_gamma[(sample - 1), pp],
                           alpha = sample_alpha[(sample - 1), pp],
                           beta = sample_beta[(sample - 1), pp],
                           kappa = sample_kappa[(sample - 1), pp])
    }
    
    # Move current updated state values to sample array
    for (pp in 1:participants) {
      sample_states[, 1:trials_participant[pp], pp, sample] <- st[[pp]]
    }
    
    # Use Hamiltonian MC to update participant's alpha and beta in parallel
    hm <- foreach(pp = 1:participants) %dopar% {
      hamiltonian_mc(states = sample_states[, 1:trials_participant[pp], pp, 
                                            sample],
                     alpha_tilde = log(sample_alpha[(sample - 1), pp]),
                     beta_tilde = log(sample_beta[(sample - 1), pp]),
                     kappa_tilde = log(sample_kappa[(sample - 1), pp]),
                     alpha_prior = c(prior_parameters$alpha[1], 
                                     prior_parameters$alpha[2]), 
                     beta_prior = c(prior_parameters$beta[1],
                                    prior_parameters$beta[2]),
                     kappa_prior = c(prior_parameters$kappa[1],
                                     prior_parameters$kappa[2]),
                     similarity = stimulus_similarity,
                     leap = n_leaps,
                     leap_size = step_size[pp])
    }
    
    
    # Transform and update participants alpha and beta
    for (pp in 1:participants) {
      sample_alpha[sample, pp] <- exp(hm[[pp]][[1]][1])
      sample_beta[sample, pp] <- exp(hm[[pp]][[1]][2])
      sample_kappa[sample, pp] <- exp(hm[[pp]][[1]][3])
      if (sample <= n_burn){
        acc[pp, count] <- hm[[pp]][[2]]  
      }
    }
    
    # Update gamma parameter
    sample_gamma[sample, ] <- gamma_update(
      initial_states = sample_states[, 1, , sample],
      gamma_prior = c(prior_parameters$gamma[1],
                      prior_parameters$gamma[2]))
    
    # Update epsilon parameter
    sample_epsilon[sample, ] <- epsilon_update(
      states_all = sample_states[, , , sample],
      responses_all = responses,
      epsilon_prior = c(prior_parameters$epsilon[1],
                        prior_parameters$epsilon[2]))
    
    # Update step size during burn in for Hamiltonian
    if ((sample %% 100) == 0 & (sample <= n_burn)) {
      step_size <- adjust_step(step_size = step_size, 
                               acceptance_prob = 
                                 rowMeans(x = acc, na.rm = TRUE),
                               target_acceptance = hmc_acceptance)
      count <- 0
    }
    
    # Update progress bar
    setTxtProgressBar(pb = progress, value = sample)
  }
  
  # Setup output
  output <- list("posterior_samples" = 
                   list("gamma" = sample_gamma[(n_burn + 1):n_iterations, ],
                        "epsilon" = sample_epsilon[(n_burn + 1):n_iterations, ],
                        "alpha" = sample_alpha[(n_burn + 1):n_iterations, ],
                        "beta" = sample_beta[(n_burn + 1):n_iterations, ],
                        "kappa" = sample_kappa[(n_burn + 1):n_iterations, ],
                        "hidden_states" = 
                          sample_states[, , , (n_burn + 1):n_iterations]),
                 "hmc_acceptance" = acc,
                 "step_size" = step_size)
  
  close(progress)
  
  return(output)
}
