################################################################################
# Function that takes the posterior samples from each block and glues them 
# together
################################################################################

block_posteriors <- function (participant_id, change_points) {
  
  output <- list()
  
  tmp <- readRDS(
    file = paste(c("data/posterior-samples/model-parameters/",
                   "three-environments-posterior-samples.rds"), 
                 collapse = ""))
  
  output$posterior_samples$hidden_states <- 
    tmp$posterior_samples$hidden_states[, , participant_id, ]
  
  for (ii in 1:3) {
      
    tmp <- readRDS(
      file = paste(c("data/posterior-samples/model-parameters/",
                     "three-environments-posterior-samples-block-", 
                     ii, ".rds"), collapse = ""))
    
      
    
    output$posterior_samples$hidden_states[, 
                                           change_points[1, ii]:
                                             change_points[1, ii + 1], 1, ] <-
      tmp$posterior_samples$hidden_states[, change_points[1, ii]:
                                            change_points[1, ii + 1], 1, ]
    
    output$posterior_samples$hidden_states[, 
                                           change_points[2, ii]:
                                             change_points[2, ii + 1], 2, ] <-
      tmp$posterior_samples$hidden_states[, change_points[2, ii]:
                                            change_points[2, ii + 1], 2, ]
    
  }
  
  return(output)
}
