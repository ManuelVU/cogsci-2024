################################################################################
# Exponential filter function
################################################################################

exponential.filter <- function(data, alpha){
  t <- length(data)
  ef <- c()
  ef[1] <- data[1]
  for(i in 2:t){
    ef[i] <- alpha * data[i] + (1-alpha) * ef[i-1]
  }
  return(ef)
}