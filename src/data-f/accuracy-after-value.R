accuracy.after.value <- function (data, domain, trials_before = 21, 
                                  trials_after = trials_before, value_before, 
                                  value_after) {
  
  output <- rep(x = NA, trials_after)
  
  ids <- sort(unique(data$id))
  
  for (ii in ids) {
    if (sum(data$change_point[data$id == ii & 
                             data$domain_char == domain]) >= 1) {
      
      output <- rbind(output, 
                      accuracy.after.value.part(data = data, participant = ii, 
                                                domain_keep = domain, 
                                                trials_before = trials_before,
                                                trials_after = trials_after, 
                                                value_before = value_before,
                                                value_after = value_after))
    }
  }
  
  return(output[-1,])
  
}