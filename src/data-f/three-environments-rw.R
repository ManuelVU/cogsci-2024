################################################################################
# Read and write function for the three environments data
################################################################################

three_env_rw <- function (data_path, domains_keep = "all", participants_keep, 
                          file_suffix) {
  # read matlab data from `data_path` argument
  tmp <- R.matlab::readMat(con = data_path)
  
  # number of participants
  n_participants <- tmp$d[[2]][1]
  
  # start variables to store values
  id <- c()
  trial <- c()
  domain <- c()
  domain_char <- c()
  trial_domain <- c()
  condition <- c()
  condition_char <- c()
  trial_condition <- c()
  stimulus <- c()
  stimulus_char <- c()
  response <- c()
  response_char <- c()
  category <- c()
  category_char <- c()
  correct <- c()
  change_point <- c()
  
  # start variable identifiers
  condition_id <- c()
  domain_id <- c()
  stimulus_id <- c()
  response_id <- c()
  category_id <- c()
  
  # un-list category, stimulus and domain variables
  domain_names <- unlist(tmp$d[[14]])
  category_names <- unlist(tmp$d[[19]])
  stimulus_names <- unlist(tmp$d[[15]])
  response_names <- unlist(tmp$d[[16]])
  
  # start loop to build a data frame using long format
  for (i in 1:n_participants) {
    # number of trials for participant i
    n_trials_part <- tmp$d[[5]][i]
    
    # update id column
    id <- append(x = id, values = rep(x = i, times = n_trials_part))
    
    # update continuous trial number
    trial <- append(x = trial, values = seq(1, n_trials_part))
    
    # read domain variable by trial
    domain_id <- tmp$d[[8]][i, 1:n_trials_part]
    
    # update domain as numeric value
    domain <- append(x = domain, values = domain_id)
    
    # update name of domain
    domain_char <- append(x = domain_char, values = domain_names[domain_id])
    
    # update trial by domain
    trial_domain <- append(x = trial_domain, 
                           values = tmp$d[[7]][i, 1:n_trials_part])
    
    # read category variable by trial
    condition_id <- tmp$d[[12]][i, 1:n_trials_part]
    
    # update category as numeric variable
    condition <- append(x = condition, values = condition_id)
    
    # update name of category
    condition_char <- append(x = condition_char, 
                            values = category_names[condition_id])
    
    # update trial by condition
    trial_condition <- append(
      x = trial_condition, 
      values = unlist(sapply(X = 
          unname(table(x = tmp$d[[12]][i, 1:n_trials_part])[
            rank(unique(tmp$d[[12]][i, 1:n_trials_part]))]), 
        FUN = seq, from = 1)))
    
    # read stimulus variable by trial
    stimulus_id <- tmp$d[[9]][i, 1:n_trials_part]
    
    # update stimulus as a numeric variable
    stimulus <- append(x = stimulus, values = stimulus_id)
    
    # update name of stimulus
    stimulus_char <- append(x = stimulus_char, 
                            values = stimulus_names[stimulus_id])
    
    # read response variable by trial
    response_id <- tmp$d[[10]][i, 1:n_trials_part]
    
    # update response as numeric value
    response <- append(x = response, values = response_id - 1)
    
    # update name of response
    response_char <- append(x = response_char, 
                            values = ifelse(test = response_id == 2,
                                            yes = "healthy", 
                                            no = "infected"))
    
    # update true category indicator
    category_id <- tmp$d[[21]][i, 1:n_trials_part]
    
    # update true category as numeric value
    category <- append(x = category, values = category_id - 1)
    
    # update true category of stimulus
    category_char <- append(x = category_char, 
                         values = ifelse(test = category_id == 2, 
                                         yes = "healthy", 
                                         no = "infected"))
    
    # update correct response variable
    correct <- append(x = correct, values = tmp$d[[11]][i, 1:n_trials_part])
    
    # update change point indicator
    change_point <- append(x = change_point, 
                           values = dplyr::lag(condition_id) != condition_id & 
                             tmp$d[[7]][i, 1:n_trials_part] != 1)
    
  }
  
  # remove change point from first trial of a domain
  change_point <- ifelse(test = is.na(change_point), 
                         yes = FALSE, no = change_point)
  
  # create output variable as a data frame
  output <- data.frame("id" = id,
                       "trial" = trial,
                       "domain" = domain,
                       "domain_char" = domain_char,
                       "trial_domain" = trial_domain, 
                       "condition" = condition,
                       "condition_char" = condition_char,
                       "trial_condition" = trial_condition,
                       "stimulus" = stimulus,
                       "stimulus_char" = stimulus_char,
                       "response" = response,
                       "response_char" = response_char,
                       "category" = category,
                       "category_char" = category_char,
                       "correct" = correct,
                       "change_point" = change_point)
  
  # filter out domains if required
  if (domains_keep != "all") {
    output <- subset(x = output, subset = output$domain_char %in% domains_keep)
    
    # filter out participants if required
    if (!missing(x = participants_keep)) {
      output <- subset(x = output, subset = output$id %in% participants_keep)
      file_suffix <- paste(c(domains_keep, "-filtered",file_suffix), collapse = "")
    }
    else {
      file_suffix <- paste(c(domains_keep, file_suffix), collapse = "")
    }
  }
  
  if (missing(file_suffix)) {
    file_suffix <- "all"  
  }
  
  # write output variable as a csv file in long format.
  readr::write_csv(x = output, 
                   file = paste(c("data/csv-files/three-environments-", 
                                  file_suffix, ".csv"), collapse = ""))
  
}
