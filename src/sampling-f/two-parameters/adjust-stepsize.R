# Function to adjust the step size of the HMC sampling algorithm

adjust_step <- function(step_size, acceptance_prob, target_acceptance){
 
  x <- c()
  new_step_size <- c()
  
  for (i in 1:length(step_size)){
    
    x[i] <- 1 + 1000 * (acceptance_prob[i] - target_acceptance) ^ 3
    
    new_step_size[i] <- ifelse(test = x[i] < 0.9, 
                               yes = step_size[i] * 0.9, 
                               no = ifelse(test = x[i] > 1.1, 
                                           yes = step_size[i] * 1.1, 
                                           no = step_size[i]))
    
  }
  
  return(new_step_size)
}
