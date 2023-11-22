################################################################################
# Plot trial by trial choices, mean posterior state and posterior parameter 
# samples of all participants in the disease domain
################################################################################

# load plotting function
source(file = "src/plot-f/plot-trial-trial.R")

# load data transformation function
source(file = "src/sampling-f/transform-data-chmm.R")

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

# color by category
color_cat <- c("#64c5eb", "#7f58af", "#e84d8a", "#feb326")

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
  cp <- which(x = subset(x = tmp, subset = id == part[pp])$change_point == TRUE)
  
# transform condition into numeric ordered variable
  condition_ordered <- sort(unique(tmp$condition_char[which(tmp$id == part[pp])]))
  
  cc <- ifelse(test = tmp$condition_char[which(tmp$id == part[pp])] %in%
                 condition_ordered[1], yes = 1, 
               no = ifelse(test = tmp$condition_char[which(tmp$id == part[pp])] %in%
                             condition_ordered[2], yes = 2, 
                           no = ifelse(
                             test = tmp$condition_char[which(tmp$id == part[pp])] %in%
                               condition_ordered[3], yes = 3, no = 4)))

# plot trial by trial responses and posterior mean in a single graph
  trial_trial_participant(data = three_env, 
                          conditions = cc,
                          posterior_add = TRUE, 
                          posteriors = samples,
                          participant_id = pp, 
                          width = 0.75, 
                          height = 0.33,
                          category_color = 
                            cbind(rep(x = "#36454f", 4), color_cat),
                          border_color = 
                            cbind(rep(x = "#36454f", 4), color_cat),
                          shade_stimulus = TRUE,
                          bar_width = 0.27,
                          transparency_bars = FALSE, 
                          lwd_rect = 1.5, change_points = cp)
  
  # add participant id as title for each plot
  mtext(text = paste("Participant: ", part[pp]), 
        side = 3, outer = TRUE, cex = 2, line = -0.2, at = 0.5)
  
  # add L shaped limits to the plotting region
  box(bty = "l")
  
  # add x-axis values making sure to add the total number of trials for the 
  # participant
  axis(1, at = c(1, seq(30,three_env$participant_t[pp], 30),
                 three_env$participant_t[pp]))
  
  # add stimulus names in the y axis of the plot
  axis(2, at = seq(1, nrow(three_env$response)) - 0.5, 
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
