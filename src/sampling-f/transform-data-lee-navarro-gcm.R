# Function transforms data from lee and navarro's experiment into a form that 
# can be used for the GCM model

transform_lee_navarro_gcm <- function (data_path, features_path) {
  output <- list()
  
  data <- readr::read_csv(file = data_path)
  
  participants <- unique(data$id)
  
  features <- readr::read_csv(file = features_path)
  
  features <- features[, -c(1,2)]
  
  distinctive_feat <- distinctive_ln(stimulus_features = features)
  
  distance_feat <- featural_distance(distinctive_features = distinctive_feat)
  
  stimulus_similarity <- similarity_ij(decay_rate = 1, decay_function = 1, 
                                       dissimilarity = distance_feat)
  
  output$distance <- distance_feat
  
  output$n_participants <- length(unique(data$id))
  
  output$n_stimulus <- length(unique(data$stimulus))
  
  output$belong_x <- matrix(data = 0, 
                            nrow = output$n_participants, 
                            ncol = output$n_stimulus)
  
  output$belong_y <- matrix(data = 0, 
                            nrow = output$n_participants, 
                            ncol = output$n_stimulus)
  
  output$n_trials <- matrix(data = NA, 
                            nrow = output$n_participants, 
                            ncol = output$n_stimulus)
  
  output$y <- matrix(data = NA, 
                     nrow = output$n_participants, 
                     ncol = output$n_stimulus)
  
  count_pp <- 0
  
  for (pp in participants) {
    count_pp <- count_pp + 1
    
    for (ss in 1:output$n_stimulus) {
      tmp <- subset(x = data, subset = id == pp & stimulus == ss)
      
      output$n_trials[count_pp, ss] <- dim(tmp)[1]
      
      output$belong_x[count_pp, ss] <- 1 - unique(tmp$category)
      output$belong_y[count_pp, ss] <- unique(tmp$category)
      
      output$y[count_pp, ss] <- sum(tmp$correct)
    }
  }
  return(output)
}
