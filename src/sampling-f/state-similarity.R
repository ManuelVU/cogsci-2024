# Effect of similarity on transition probabilities.
state_similarity <- function(states_vec, similarity_mat, method = "average") {
  
  states_vec <- 2 * states_vec - 1
  
  if (method == "total") {
    
    return(similarity_mat %*% states_vec)
    
  } else if (method == "average") {

    w_1 <- ifelse(test = sum(states_vec == 1) > 0, 
                  yes = 1 / sum(states_vec == 1), 
                  no = 0)
    w_0 <- ifelse(test = sum(states_vec == - 1) > 0,
                  yes = 1 / sum(states_vec == -1),
                  no = 0)
    
    states_vec <- w_1 * (states_vec == 1)  + w_0 * (states_vec == - 1)
    
    return(similarity_mat %*% states_vec)
  }
}

# On ffbs 
# relative_sim_others <- similarity_to_others %*%
#   (2 * states_rest[, (t - 1)] - 1)

# On transition-others
# relative_similarity <-  cbind(similarity %*% (2 * state_all[, 1] - 1),
#                               similarity %*% (2 * state_all[, 2] - 1))

# On sample-prior-states
# relative_sim_others <- similarity_to_others %*%
#   (2 * output[, (t - 1), p] - 1)

# On gradient-inertia
# relative_sim_others <- similarity_to_others %*%
#   (2 * states_rest[, (t - 1)] - 1)

# log-posterior
# relative_sim_others <- similarity_to_others %*%
#   (2 * states_rest[, (t - 1)] - 1)

# Test
# state_similarity(states_vec = c(1, 1, 0, 1, 0, 0, 1, 1),
#                  similarity_mat = c(0.7165313, 0.7165313, 0.7165313, 
#                                     0.5134171, 0.5134171, 0.7165313, 0.5134171,
#                                     0.5134171), method = "average")
