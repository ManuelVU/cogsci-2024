accuracy.before <- function (data, domain, value = "positive", 
                             trials_before = 21) {
  
  ids <- unique(data$id)
  
  out <- seq(1, trials_before)
  
  source(file = "src/data-f/accuracy-before-participant.R")
  
  for (kk in ids) {
    
    if (sum(data$change_point[data$id == kk & 
                              data$domain_char == domain]) >= 1) {
      
      out <- rbind(out, 
                   accuracy.before.participant(data = data, 
                                               participant = kk, 
                                               domain_keep = domain,
                                               value = value,
                                               trials_before = trials_before))
      
    }
  }
  
  return(out[-1, ])
  
}