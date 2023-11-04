################################################################################
# Read and write function for the three environments data
################################################################################

three_env_rw <- function (data_path, conditions = "all", participants_keep, 
                          files_suffix) {
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
    category_id <- tmp$d[[12]][i, 1:n_trials_part]
    
    # update category as numeric variable
    category <- append(x = category, values = category_id)
    
    # update name of category
    category_char <- append(x = category_char, 
                            values = category_names[category_id])
    
    # update trial by condition
    trial_condition <- append(
      x = trial_condition, 
      values = unlist(sapply(X = 
          unname(table(x = tmp$d[[12]][i, 1:n_trials_part])[
            rank(unique(tmp$d[[12]][i, 1:n_trials_part]))]), 
        FUN = seq, from = 1)))
    
    # read stimulus variable by trial
    stimulus_id <- three_env$d[[9]][i, 1:n_trials_part]
    
    # update stimulus as a numeric variable
    stimulus <- append(x = stimulus, values = stimulus_id)
    
    # update name of stimulus
    stimulus_char <- append(x = stimulus_char, 
                            values = stimulus_names[stimulus_id])
    
    # read response variable by trial
    response_id <- tmp$d[[10]][i, 1:n_trials]
    
    # update response as numeric value
    response <- append(x = response, values = response_id - 1)
    
    # update name of response
    response_char <- append(x = response_char, 
                            values = 
                              response_names[response_id + 2 * domain_id - 2])
    
    
    
  }

}

for(i in 1:n){
  t_id <- three_env$d[[21]][i, 1:n_trials]
  truth_id <- append(x = truth_id, values = t_id - 1)
  truth <- append(x = truth, values = r_names[t_id + 2 * d_id - 2])
  correct <- append(x = correct, values = three_env$d[[11]][i, 1:n_trials])
  block <- append(x = block, values = three_env$d[[13]][i, 1:n_trials])
  change_point <- append(x = change_point, 
                         values = lag(c_id) != c_id & three_env$d[[7]][i, 1:n_trials] != 1)
}

change_point <- ifelse(test = is.na(change_point), yes = FALSE, 
                       no = change_point)

three_env <- data.frame("id" = id, "trial" = trial, "trial_domain" = trial_domain, 
                        "domain_id" = domain_id, "domain" = domain,
                        "category_id" = category_id, "category" = category,
                        "stimulus_id" = stimulus_id, "stimulus" = stimulus, 
                        "response_id" = response_id, "response" = response, 
                        "truth_id" = truth_id, "truth" = truth, 
                        "correct" = correct, "block" = block,
                        "change_point" = change_point,
                        "condition" = rep(1, length(id)))

write_csv(three_env, file = "data/three-environments/three-environments.csv")
