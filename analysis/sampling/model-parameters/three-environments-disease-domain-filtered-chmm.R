################################################################################
# Sample parameters of the CHMM model using the filtered data from the disease
# domain in the three environments experiment
################################################################################

# load sampling and similarity functions into R environment
source(file = "analysis/load-functions.R")

# transform the filtered disease domain data into the chmm format
three_env <- transform_data_chmm(
  directory_data = "data/csv-files/three-environments-Disease-filtered.csv",
  directory_features = "data/stimuli-features/mds-fast-1-and-2.csv")

# set number of iterations for the sampler
iterations <- 15000

# set number of burn in samples 
# (this will be used as an adaptation period by the HMC algorithm)
burn <- 10000

# set target acceptance ratio for HMC algorithm
acceptance_target <- 0.8

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
                                     times = dim(three_env$response)[3]),
                       "epsilon" = rbeta(n = dim(three_env$response)[3],
                                         shape1 = 10,
                                         shape2 = 100),
                       "alpha" = rep(x = 5, 
                                     times = dim(three_env$response)[3]),
                       "beta" = rep(x = 5, 
                                    times = dim(three_env$response)[3]))

# set step size for the adaptation method of the HMC algorithm
step_size_starting <- rep(0.0015, dim(three_env$response)[3])

# Start sampling using chmm_sampler
samples <- chmm_sampling(data_chmm = three_env,
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
        file = "data/posterior-samples/three-environments-posterior-samples.rds")
