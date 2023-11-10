################################################################################
# Plot trial by trial choices and mean posterior state for a single participant
# and all stimuli in the disease domain
################################################################################

# load ploting function
source(file = "src/plot-f/plot-trial-trial.R")

# load data transformed into the chmm format with features
three_env <- transform_data_chmm(
  directory_data = "data/csv-files/three-environments-Disease-filtered.csv",
  directory_features = "data/stimuli-features/mds-fast-1-and-2.csv")

# load original data file to find change points
tmp <- readr::read_csv(
  file = "data/csv-files/three-environments-Disease-filtered.csv")

# load posterior samples for CHMM model parameters
samples <- readRDS(file = paste(c("data/posterior-samples/model-parameters/",
                                  "three-environments-posterior-samples.rds"),
                                collapse = ""))

# start figure pdf file
grDevices::cairo_pdf(
  file = paste(c("figures/trial-trial-plot-three-env-participant-", pp, ".pdf"), 
               collapse = ""),
  width = 8.3, height = 6)

# set participant id to plot
pp <- 3

# find change points of participant pp
change_points <- which(
  x = subset(x = tmp, subset = id == pp)$change_point == TRUE)

# organize iner and outer margins for plot
par(oma = c(2.5,6,1,0.7),
    mai = c(0.1,0.2,0.1,0),
    yaxs = "i", 
    xaxs = "i")

# plot trial by trial responses and posterior mean in a single graph
trial_trial_participant(data = three_env, 
                        posterior_add = TRUE, 
                        posteriors = samples,
                        participant_id = pp, 
                        width = 0.75, 
                        height = 0.33,
                        category_color = c("#30626d", "#723f75"),
                        border_color = c("#30626d", "#723f75"), 
                        shade_stimulus = TRUE,
                        bar_width = 0.27,
                        transparency_bars = FALSE, 
                        lwd_rect = 1.5)

# add vertical lines to the plot to highlight change points
abline(v = change_points)

# add L shaped limits to the plotting region
box(bty = "l")

# add x-axis values making sure to add the total number of trials for the 
# participant
axis(1, at = c(1, seq(30,three_env$participant_t[pp], 30),
               three_env$participant_t[pp]))

# add stimulus names in the y axis of the plot
axis(2, at = seq(1, nrow(three_env$response)), 
     labels = three_env$stimulus_features$animal_names, las = 2)

dev.off()
