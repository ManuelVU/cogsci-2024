################################################################################
# Function that computes a moving average with a fixed window size
################################################################################

moving.average <- function(data, window_size){
  t <- length(data)
  ma <- c()
  for(i in 1:t){
    if(i < window_size){
      ma[i] <- mean(data[1:i]) 
    }
    else{
      ma[i] <- mean(data[(i - window_size + 1):i])
    }
  }
  return(ma)
}