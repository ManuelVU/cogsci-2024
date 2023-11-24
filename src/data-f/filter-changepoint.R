################################################################################
# Function filters data between change-points
################################################################################

filter.changepoint <- function(data, participant, category_keep, domain_keep, 
                               filter = "ma", window_size = 10, alpha = NA){
  
  source(file = "src/data-f/pull-part-category-domain.R")
  source(file = "src/data-f/moving-average.R")
  source(file = "src/data-f/exponential-filter.R")
  
  q <- pull.part.category.domain(data = data, 
                                 participant = participant, 
                                 category_keep = category_keep,
                                 domain_keep = domain_keep)
  
  q$change_point[1] <- FALSE
  
  n_cp <- sum(q$change_point)
  
  filtered_acc <- c()
  
  if(filter == "ma"){
    
    stopifnot(window_size %% 1 == 0)
    
    if(n_cp > 0){
      
      t_cp <- c(1, which(q$change_point == TRUE), dim(q)[1] + 1)
      
      for(tt in 2:length(t_cp)){
        
        tmp_data <- q[t_cp[tt-1]:(t_cp[tt]-1), ]
        
        filtered_acc <- append(x = filtered_acc, 
                               values = moving.average(data = tmp_data$correct, 
                                                       window_size = window_size))
      }
    }
    else{
      filtered_acc <- moving.average(data = q$correct, 
                                     window_size = window_size)
    }
  }
  
  if(filter == "exponential"){
    
    stopifnot(is.numeric(alpha))
    
    if(n_cp > 0){
      
      t_cp <- c(1, which(q$change_point == TRUE), dim(q)[1])
      
      for(tt in 2:length(t_cp)){
        
        tmp_data <- q[t_cp[tt-1]:(t_cp[tt]-1), ]
        
        filtered_acc <- append(x = filtered_acc, 
                               values = 
                                 exponential.filter(data = tmp_data$correct, 
                                                    alpha = alpha))
      }
    }
    else{
      filtered_acc <- exponential.filter(data = q$correct, alpha = alpha)
    }
  }
  
  return(data.frame("trial_domain" = q$trial_domain,
                    "filtered_acc" = filtered_acc, 
                    "change_point" = q$change_point))
  
}