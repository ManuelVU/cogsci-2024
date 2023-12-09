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
    
    states_vec <- w_1 * (states_vec == 1) - w_0 * (states_vec == - 1)
    
    return(similarity_mat %*% states_vec)
  }
}
