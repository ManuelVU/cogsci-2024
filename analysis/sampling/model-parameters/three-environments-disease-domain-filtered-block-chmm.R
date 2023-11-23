################################################################################
# Sampling by trial blocks before and after change points
################################################################################
# Load functions into R environment
source(file = "analysis/load-functions.R")

# wirte csv file with only two participants (best and worst)
source(file = "src/data-f/three-environments-rw.R")
three_env_rw(data_path = "data/matlab-files/three-environments.mat", 
             domains_keep = "Disease", participants_keep = c(2, 16),
             file_suffix = "-block-sampling")

# load data from csv
three_environments <- transform_data_chmm(
  directory_data = "data/csv-files/three-environments-Disease-filtered-block-sampling.csv",
  directory_features = "data/stimuli-features/mds-fast-1-and-2.csv")

tmp <- readr::read_csv(file = "data/csv-files/three-environments-Disease-filtered-block-sampling.csv")

cp_1 <- unique(subset(x = tmp,
                      subset = change_point == TRUE & id == 2)$trial_domain)

cp_2 <- unique(subset(x = tmp,
                      subset = change_point == TRUE & id == 16)$trial_domain)

disease <- list()

# change-point 1 ----
disease$response <- array(data = NA, 
                          dim = c(dim(three_environments$response)[1],
                                  dim(three_environments$response)[2],
                                  dim(three_environments$response)[3]))

disease$response[,1:(cp_1[1] - 1), 1] <- 
  three_environments$response[, 1:(cp_1[1] - 1), 1]

disease$response[, 1:(cp_2[1] - 1), 2] <- 
  three_environments$response[, 1:(cp_2[1] - 1), 2]

disease$participant_t <- c(cp_1[1] - 1, cp_2[1] - 1)

disease$stimulus_features <- three_environments$stimulus_features

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
                     "beta" = c(2, 1))

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
saveRDS(object = samples, 
        file = "data/posterior-samples/three-environments-posterior-samples-block-1.rds")

# change-point 2 ----
disease$response <- array(data = NA, 
                          dim = c(dim(three_environments$response)[1],
                                  dim(three_environments$response)[2],
                                  dim(three_environments$response)[3]))

disease$response[, 1:(cp_1[2] - 1), 1] <- 
  three_environments$response[, 1:(cp_1[2] - 1), 1]

disease$response[, 1:(cp_2[2] - 1), 2] <- 
  three_environments$response[, 1:(cp_2[2] - 1), 2]

disease$participant_t <- c(cp_1[2] - 1, cp_2[2] - 1)

disease$stimulus_features <- three_environments$stimulus_features

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
                     "beta" = c(2, 1))

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
                                    times = dim(disease$response)[3]))

# set step size for the adaptation method of the HMC algorithm
step_size_starting <- rep(x = 0.1, times = dim(disease$response)[3])

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
saveRDS(object = samples, 
        file = "data/posterior-samples/three-environments-posterior-samples-block-2.rds")

# change-point 3 ----
disease$response <- array(data = NA, 
                          dim = c(dim(three_environments$response)[1],
                                  dim(three_environments$response)[2],
                                  dim(three_environments$response)[3]))

disease$response[,1:(cp_1[3] - 1), 1] <- 
  three_environments$response[, 1:(cp_1[3] - 1), 1]

disease$response[, 1:(cp_2[3] - 1), 2] <- 
  three_environments$response[, 1:(cp_2[3] - 1), 2]

disease$participant_t <- c(cp_1[3] - 1, cp_2[3] - 1)

disease$stimulus_features <- three_environments$stimulus_features

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
                     "beta" = c(2, 1))

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
saveRDS(object = samples, 
        file = "data/posterior-samples/three-environments-posterior-samples-block-3.rds")

