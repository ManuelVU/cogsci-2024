# Function that calculates the similarity between stimulus i and j using the 
# featural distance metric (Lee and Navarro, 2002) this function takes two
# arguments, a decay rate (decay, aka sigma) and a dissimilarity matrix.
similarity_ij <- function(decay_rate = 1, decay_function = 1, dissimilarity){
  exp(-decay_rate * (dissimilarity ^ decay_function))
} 
