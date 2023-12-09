# function that computes the logit of x

logit <- function(x){
  max(0.00001, min(0.99999, 1 / (1 + exp(- x))))
} 
