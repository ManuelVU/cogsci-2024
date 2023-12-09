################################################################################
# Function that takes the posterior samples from each block and glues them 
# together
################################################################################

block_posteriors <- function (participant_id,
                              start_points,
                              end_points,
                              parameters = 2,
                              similarity = TRUE) {
  
  output <- list()
  
  if (parameters == 2) {
    direct <- "two-parameters/"
      
    suffix <- ""
  } else {
    direct <- "three-parameters/"
    
    suffix <- "-chmm-3param"
  }
  
  tmp <- readRDS(
    file = paste(c("data/posterior-samples/model-parameters/",
                   direct,
                   "three-environments-posterior-samples-block-4-similarity",
                   suffix,
                   ".rds"), 
                 collapse = ""))
  
  output <- tmp
  
  for (ii in 1:3) {
      
    tmp <- readRDS(
      file = paste(c("data/posterior-samples/model-parameters/",
                     direct,
                     "three-environments-posterior-samples-block-",
                     ii,
                     "-similarity",
                     suffix,
                     ".rds"),
                   collapse = ""))
    
    output$posterior_samples$hidden_states[, start_points[1, ii]:
                                             end_points[1, ii], 1, ] <-
      tmp$posterior_samples$hidden_states[, start_points[1, ii]:
                                            end_points[1, ii], 1, ]
    
    output$posterior_samples$hidden_states[, start_points[2, ii]:
                                             end_points[2, ii], 2, ] <-
      tmp$posterior_samples$hidden_states[, start_points[2, ii]:
                                            end_points[2, ii], 2, ]
    
  }
  
  return(output)
}
