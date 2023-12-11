before.after.stimulus <- function (stimulus_before, stimulus_after) {
  
  return(stimulus_after[which(stimulus_after %in% stimulus_before)])
  
}