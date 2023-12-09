# Load functions into R environment
source(file = "analysis/load-3parameter-functions.R")

# load data from csv into the chmm format
three_environments <- transform_data_chmm(
  directory_data = 
    paste(c("data/csv-files/",
            "three-environments-Disease-filtered-block-sampling.csv"),
          collapse = ""),
  directory_features = "data/stimuli-features/fast_1_2_similarity_matrix.csv",
  similarity = TRUE)

# load full data file to find change points
tmp <- readr::read_csv(
  file = paste(c("data/csv-files/",
                 "three-environments-Disease-filtered-block-sampling.csv"),
               collapse = ""))

# change points for participant 1
cp_1 <- unique(subset(x = tmp,
                      subset = change_point == TRUE & id == 2)$trial_domain)

# change points for participant 2
cp_2 <- unique(subset(x = tmp,
                      subset = change_point == TRUE & id == 16)$trial_domain)


# change-point 1 ----
# remove list if it already exists
rm(list = "disease")

# new list containing only data from the start of the session to the current
# change point
disease <- list()

disease$response <- array(data = NA, 
                          dim = c(dim(three_environments$response)[1],
                                  dim(three_environments$response)[2],
                                  dim(three_environments$response)[3]))

disease$response[,1:(cp_1[1] - 1), 1] <- 
  three_environments$response[, 1:(cp_1[1] - 1), 1]

disease$response[, 1:(cp_2[1] - 1), 2] <- 
  three_environments$response[, 1:(cp_2[1] - 1), 2]

disease$participant_t <- c(cp_1[1] - 1, cp_2[1] - 1)

disease$similarity <- three_environments$similarity

# set number of iterations for the sampler
iterations <- 15000

# set number of burn in samples 
# (this will be used as an adaptation period by the HMC algorithm)
burn <- 10000

# set target acceptance ratio for HMC algorithm
acceptance_target <- 0.7

# set number of cores used for parallel computing
cores <- as.integer(round(x = parallel::detectCores() / 2, digits = 0))

# set values of the prior distributions for the sampler, distributions used 
# are explained in the paper
prior_values <- list("gamma" = c(1, 1),
                     "epsilon" = c(2, 102),
                     "alpha" = c(2, 1), 
                     "beta" = c(2, 1),
                     "kappa" = c(2, 1))

# set initial values for the chains, the initial values for the hidden states 
# are started by the sampler itself
initial_values <- list("gamma" = rep(x = 0.5, 
                                     times = dim(disease$response)[3]),
                       "epsilon" = rbeta(n = dim(disease$response)[3],
                                         shape1 = 2,
                                         shape2 = 102),
                       "alpha" = rep(x = 5, 
                                     times = dim(disease$response)[3]),
                       "beta" = rep(x = 5, 
                                    times = dim(disease$response)[3]),
                       "kappa" = rep(x = 1, 
                                     times = dim(disease$response)[3]))

# set step size for the adaptation method of the HMC algorithm
step_size_starting <- rep(0.1, dim(disease$response)[3])

# Start sampling using chmm_sampler
samples <- chmm_sampling(data_chmm = disease,
                         metric = "minkowski",
                         order_p = 1,
                         n_iterations = iterations,
                         n_burn = burn,
                         n_cores = cores,
                         parameters_initial_values = initial_values,
                         prior_parameters = prior_values,
                         start_step_size = step_size_starting, 
                         hmc_acceptance = acceptance_target)

# Save posterior samples
saveRDS(
  object = samples, 
  file = 
    paste(c("data/posterior-samples/model-parameters/",
            "three-environments-posterior-samples-block-1-similarity-chmm-",
            "3param.rds"), collapse = ""))



# change-point 2 ----
# remove list if it already exists
rm(list = "disease")

# new list containing only data from the start of the session to the current
# change point
disease <- list()

disease$response <- array(data = NA, 
                          dim = c(dim(three_environments$response)[1],
                                  dim(three_environments$response)[2],
                                  dim(three_environments$response)[3]))

disease$response[,1:(cp_1[2] - 1), 1] <- 
  three_environments$response[, 1:(cp_1[2] - 1), 1]

disease$response[, 1:(cp_2[2] - 1), 2] <- 
  three_environments$response[, 1:(cp_2[2] - 1), 2]

disease$participant_t <- c(cp_1[2] - 1, cp_2[2] - 1)

disease$similarity <- three_environments$similarity

# set number of iterations for the sampler
iterations <- 15000

# set number of burn in samples 
# (this will be used as an adaptation period by the HMC algorithm)
burn <- 10000

# set target acceptance ratio for HMC algorithm
acceptance_target <- 0.7

# set number of cores used for parallel computing
cores <- as.integer(round(x = parallel::detectCores() / 2, digits = 0))

# set values of the prior distributions for the sampler, distributions used 
# are explained in the paper
prior_values <- list("gamma" = c(1, 1),
                     "epsilon" = c(2, 102),
                     "alpha" = c(2, 1), 
                     "beta" = c(2, 1),
                     "kappa" = c(2, 1))

# set initial values for the chains, the initial values for the hidden states 
# are started by the sampler itself
initial_values <- list("gamma" = rep(x = 0.5, 
                                     times = dim(disease$response)[3]),
                       "epsilon" = rbeta(n = dim(disease$response)[3],
                                         shape1 = 2,
                                         shape2 = 102),
                       "alpha" = rep(x = 5, 
                                     times = dim(disease$response)[3]),
                       "beta" = rep(x = 5, 
                                    times = dim(disease$response)[3]),
                       "kappa" = rep(x = 1, 
                                     times = dim(disease$response)[3]))

# set step size for the adaptation method of the HMC algorithm
step_size_starting <- rep(0.1, dim(disease$response)[3])

# Start sampling using chmm_sampler
samples <- chmm_sampling(data_chmm = disease,
                         metric = "minkowski",
                         order_p = 1,
                         n_iterations = iterations,
                         n_burn = burn,
                         n_cores = cores,
                         parameters_initial_values = initial_values,
                         prior_parameters = prior_values,
                         start_step_size = step_size_starting, 
                         hmc_acceptance = acceptance_target)

# Save posterior samples
saveRDS(
  object = samples, 
  file = 
    paste(c("data/posterior-samples/model-parameters/",
            "three-environments-posterior-samples-block-2-similarity-chmm-",
            "3param.rds"), collapse = ""))



# change-point 3 ----
# remove list if it already exists
rm(list = "disease")

# new list containing only data from the start of the session to the current
# change point
disease <- list()

disease$response <- array(data = NA, 
                          dim = c(dim(three_environments$response)[1],
                                  dim(three_environments$response)[2],
                                  dim(three_environments$response)[3]))

disease$response[,1:(cp_1[3] - 1), 1] <- 
  three_environments$response[, 1:(cp_1[3] - 1), 1]

disease$response[, 1:(cp_2[3] - 1), 2] <- 
  three_environments$response[, 1:(cp_2[3] - 1), 2]

disease$participant_t <- c(cp_1[3] - 1, cp_2[3] - 1)

disease$similarity <- three_environments$similarity

# set number of iterations for the sampler
iterations <- 15000

# set number of burn in samples 
# (this will be used as an adaptation period by the HMC algorithm)
burn <- 10000

# set target acceptance ratio for HMC algorithm
acceptance_target <- 0.7

# set number of cores used for parallel computing
cores <- as.integer(round(x = parallel::detectCores() / 2, digits = 0))

# set values of the prior distributions for the sampler, distributions used 
# are explained in the paper
prior_values <- list("gamma" = c(1, 1),
                     "epsilon" = c(2, 102),
                     "alpha" = c(2, 1), 
                     "beta" = c(2, 1),
                     "kappa" = c(2, 1))

# set initial values for the chains, the initial values for the hidden states 
# are started by the sampler itself
initial_values <- list("gamma" = rep(x = 0.5, 
                                     times = dim(disease$response)[3]),
                       "epsilon" = rbeta(n = dim(disease$response)[3],
                                         shape1 = 2,
                                         shape2 = 102),
                       "alpha" = rep(x = 5, 
                                     times = dim(disease$response)[3]),
                       "beta" = rep(x = 5, 
                                    times = dim(disease$response)[3]),
                       "kappa" = rep(x = 1, 
                                     times = dim(disease$response)[3]))

# set step size for the adaptation method of the HMC algorithm
step_size_starting <- rep(0.1, dim(disease$response)[3])

# Start sampling using chmm_sampler
samples <- chmm_sampling(data_chmm = disease,
                         metric = "minkowski",
                         order_p = 1,
                         n_iterations = iterations,
                         n_burn = burn,
                         n_cores = cores,
                         parameters_initial_values = initial_values,
                         prior_parameters = prior_values,
                         start_step_size = step_size_starting, 
                         hmc_acceptance = acceptance_target)

# Save posterior samples
saveRDS(
  object = samples, 
  file = 
    paste(c("data/posterior-samples/model-parameters/",
            "three-environments-posterior-samples-block-3-similarity-chmm-",
            "3param.rds"), collapse = ""))

# all trials ----

# load participants data
three_environments <- transform_data_chmm(
  directory_data = "data/csv-files/three-environments-Disease-filtered-block-sampling.csv",
  directory_features = "data/stimuli-features/fast_1_2_similarity_matrix.csv",
  similarity = TRUE)

# set number of iterations for the sampler
iterations <- 15000

# set number of burn in samples 
# (this will be used as an adaptation period by the HMC algorithm)
burn <- 10000

# set target acceptance ratio for HMC algorithm
acceptance_target <- 0.7

# set number of cores used for parallel computing
cores <- as.integer(round(x = parallel::detectCores() / 2, digits = 0))

# set values of the prior distributions for the sampler, distributions used 
# are explained in the paper
prior_values <- list("gamma" = c(1, 1),
                     "epsilon" = c(2, 102),
                     "alpha" = c(2, 1), 
                     "beta" = c(2, 1),
                     "kappa" = c(2, 1))

# set initial values for the chains, the initial values for the hidden states 
# are started by the sampler itself
initial_values <- list("gamma" = rep(x = 0.5, 
                                     times = dim(three_environments$response)[3]),
                       "epsilon" = rbeta(n = dim(three_environments$response)[3],
                                         shape1 = 2,
                                         shape2 = 102),
                       "alpha" = rep(x = 5, 
                                     times = dim(three_environments$response)[3]),
                       "beta" = rep(x = 5, 
                                    times = dim(three_environments$response)[3]),
                       "kappa" = rep(x = 1, 
                                     times = dim(three_environments$response)[3]))

# set step size for the adaptation method of the HMC algorithm
step_size_starting <- rep(0.1, dim(three_environments$response)[3])

# Start sampling using chmm_sampler
samples <- chmm_sampling(data_chmm = three_environments,
                         metric = "minkowski",
                         order_p = 1,
                         n_iterations = iterations,
                         n_burn = burn,
                         n_cores = cores,
                         parameters_initial_values = initial_values,
                         prior_parameters = prior_values,
                         start_step_size = step_size_starting, 
                         hmc_acceptance = acceptance_target)

# Save posterior samples
saveRDS(
  object = samples, 
  file = paste(c("data/posterior-samples/model-parameters/","
                 three-environments-posterior-samples-block-4-similarity-chmm-",
                 "3param.rds"), collapse = ""))
