accuracy.after.value.part <- function (data, participant, domain_keep, 
                                       trials_before, 
                                       trials_after = trials_before, 
                                       value_before, value_after){
  
  q <- subset(x = data, subset = id == participant) |>
    subset(subset = domain_char == domain_keep)
  
  change_points <- which(q$change_point == TRUE) - 1
  
  n_ch <- length(change_points)
  
  out <- matrix(NA, nrow = n_ch, ncol = trials_before)
  
  value_before <- ifelse(test = value_before == "positive", 
                         yes = unique(q$category_char[q$category == 0]),
                         no = unique(q$category_char[q$category == 1]))
  
  value_after <- ifelse(test = value_after == "positive", 
                        yes = unique(q$category_char[q$category == 0]),
                        no = unique(q$category_char[q$category == 1]))
  
  for (tt in 1:n_ch) {
    st_before <- unique(
      data$stimulus[data$domain_char == domain_keep &
                    data$condition_char == 
                        q$condition_char[change_points[tt]] &
                    data$category_char == value_before])
    
    st_after <- unique(
      data$stimulus[data$domain_char == domain_keep &
                    data$condition_char == 
                      q$condition_char[(change_points[tt] + 1)] &
                    data$category_char == value_after])
    
    end_point <- ifelse(test = change_points[tt] + trials_after > dim(q)[1],
                        yes = dim(q)[1], no = change_points[tt] + trials_after)
    
    d_after <- q[(change_points[tt] + 1):end_point, ]
    
    indecies <- before.after.stimulus(stimulus_before = st_before, 
                                      stimulus_after = st_after)
    
    if (!is.null(indecies)) {
      
      positions <- which(d_after$stimulus %in% indecies)
      
      for (jj in positions) {
        
        out[tt,jj] <- d_after$correct[jj]
        
      }
    }
  }
  
  return(out)
  
}