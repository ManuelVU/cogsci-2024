accuracy.before.participant <- function (data, participant, domain_keep,
                                         trials_before, value) {
  
  q <- subset(x = data, subset = id == participant) |>
    subset(subset = domain_char == domain_keep)
  
  change_points <- which(q$change_point == TRUE) - 1
  
  n_chp <- length(change_points)
  
  out <- matrix(NA, nrow = n_chp, ncol = trials_before)
  
  type <- ifelse(test = value == "positive", 
                 yes = unique(q$category_char[q$category == 0]),
                 no = unique(q$category_char[q$category == 1]))
  
  for (tt in 1:n_chp) {
    
    st_point <- ifelse(test = (change_points[tt] - trials_before + 1) > 0,
                       yes = change_points[tt] - trials_before + 1, 
                       no = 1)
    
    end_point <- ifelse(test = (st_point + trials_before - 1) > dim(q)[1],
                        yes = dim(q)[1], 
                        no = trials_before - 1)
    
    for (jj in 0:end_point) {
      
      if (q$category_char[st_point + jj] == type) {
        
        out[tt, (jj + 1)] <- q$correct[st_point + jj]
        
    } else {
      
        out[tt, (jj + 1)] <- out[tt, (jj + 1)]
        
      } 
    }
  }
  
  return(out)
  
}