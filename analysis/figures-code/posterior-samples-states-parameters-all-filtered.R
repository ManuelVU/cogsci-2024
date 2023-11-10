
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
  file = paste(c("figures/trial-trial-plot-three-env-all-states-parameters.pdf"), 
               collapse = ""),
  width = 8.3, height = 8, onefile = TRUE)

# set participant id to plot
part <- unique(tmp$id)

for (pp in 1:length(part)) {
  layout(rbind(c(1, 1, 1, 1),
               c(1, 1, 1, 1),
               c(1, 1, 1, 1),
               c(1, 1, 1, 1),
               c(1, 1, 1, 1),
               c(2, 3, 4, 5),
               c(2, 3, 4, 5),
               c(2, 3, 4, 5)))

  par(oma = c(1.5,4.7,2.2,0.7),
      mai = c(0.4,0.5,0.1,0),
      yaxs = "i", 
      xaxs = "i")
  
# find change points of participant pp
  change_points <- which(
    x = subset(x = tmp, subset = id == part[pp])$change_point == TRUE)

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
  
  # add participant id as title for each plot
  mtext(text = paste("Participant: ", pp), 
        side = 3, outer = TRUE, cex = 2, line = -0.2, at = 0.5)
    
  
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
       labels = three_env$stimulus_features$animal_names, las = 2, 
       cex.axis = 1.5)
  
  hist(samples$posterior_samples$gamma[, pp], xlim = c(0, 1), axes = FALSE,
       ann = FALSE, border = "#FCF6F5", col = "#30626d", 
       breaks = seq(0, 1, 0.05))
  box(bty = "l")
  axis(side = 1, at = c(0,0.2,0.5,0.8,1), cex.axis = 1.4,
       labels = (c("0", 0.2, 0.5, 0.8, "1")), padj = -0.2)
  mtext(text = expression(gamma), side = 1, line = 3, cex = 1.6)
  
  hist(samples$posterior_samples$epsilon[, pp], xlim = c(0,0.4), axes = FALSE,
       ann = FALSE, border = "#FCF6F5", col = "#723f75", 
       breaks = seq(0,0.4,0.02))
  box(bty = "l")
  axis(side = 1, at = seq(0,0.4,0.1), cex.axis = 1.4,
       labels = (c("0", seq(0.1,0.4,0.1))), padj = -0.2)
  mtext(text = expression(epsilon), side = 1, line = 3, cex = 1.6)
  
  hist(samples$posterior_samples$alpha[, pp], xlim = c(2,8), axes = FALSE,
       ann = FALSE, border = "#FCF6F5", col = "#30626d",
       breaks = seq(2,8,0.25))
  box(bty = "l")
  axis(side = 1, at = c(2, 4, 6, 8), cex.axis = 1.4, padj = -0.2)
  mtext(text = expression(beta[0]), side = 1, line = 3, cex = 1.6)
  
  hist(samples$posterior_samples$beta[, pp], xlim = c(2,8), axes = FALSE,
       ann = FALSE, border = "#FCF6F5", col = "#723f75",
       breaks = seq(2,8,0.25))
  box(bty = "l")
  axis(side = 1, at = c(2, 4, 6, 8), cex.axis = 1.4, padj = -0.2)
  mtext(text = expression(beta[1]), side = 1, line = 3, cex = 1.6)
  
}

dev.off()
