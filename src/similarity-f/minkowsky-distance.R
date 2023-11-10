# Function a continuous stimulus-features matrix and returns a matrix of size
# n_stimulus x n_stimulus with the Minkowski of order (p) distance from 
# the stimulus indexed by the row number and the stimulus indexed by the column
minkowski_distance <- function (stimulus_features, p) {
  
  feature_values <- cbind(stimulus_features$feature_1, 
                          stimulus_features$feature_2)
  
  n_stimulus <- nrow(feature_values)
  
  d_ij <- matrix(data = NA, nrow = n_stimulus, ncol = n_stimulus)
  
  for (i in 1:n_stimulus) {
    for (j in 1:n_stimulus) {
      
      d_ij[i, j] <- sum(abs(feature_values[i, ] - feature_values[j, ]) ^ p)
      
      d_ij[i, j] <- d_ij[i, j] ^ (1 / p)
    
    }
  }
  
  return(d_ij)
  
}
