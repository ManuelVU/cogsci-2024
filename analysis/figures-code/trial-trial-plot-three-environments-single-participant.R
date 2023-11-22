################################################################################
# Plot trial by trial choices and mean posterior state for a single participant
# and all stimuli in the disease domain
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

# set participant id to plot
pp <- 3

# participant id in general data file
pp_id <- unique(tmp$id)[pp]

# transform condition into numeric ordered variable
condition_ordered <- sort(unique(tmp$condition_char[which(tmp$id == pp_id)]))

cc <- ifelse(test = tmp$condition_char[which(tmp$id == pp_id)] %in%
               condition_ordered[1], yes = 1, 
             no = ifelse(test = tmp$condition_char[which(tmp$id == pp_id)] %in%
                           condition_ordered[2], yes = 2, 
                         no = ifelse(
                           test = tmp$condition_char[which(tmp$id == pp_id)] %in%
                             condition_ordered[3], yes = 3, no = 4)))

# category colors
color_cat <- c("#64c5eb", "#7f58af", "#e84d8a", "#feb326")

# start figure pdf file
grDevices::cairo_pdf(
  file = paste(c("figures/trial-trial-plot-three-env-participant-", pp_id, ".pdf"), 
               collapse = ""),
  width = 8.3, height = 6)


# find change points of participant pp
cp <- which(x = subset(x = tmp, subset = id == pp_id)$change_point == TRUE)

# organize inner and outer margins for plot
par(oma = c(2.5,4.8,1,0.7),
    mai = c(0.1,0.2,0.1,0),
    yaxs = "i", 
    xaxs = "i")

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
                        border_color = cbind(rep(x = "#36454f", 4), color_cat),
                        shade_stimulus = TRUE,
                        bar_width = 0.27,
                        transparency_bars = FALSE, 
                        lwd_rect = 1.5, 
                        change_points = cp)

# add L shaped limits to the plotting region
box(bty = "l")

# add x-axis values making sure to add the total number of trials for the 
# participant
axis(1, at = c(1, seq(30,three_env$participant_t[pp], 30),
               three_env$participant_t[pp]), padj = -0.9, cex = 1.3)

# add stimulus names in the y axis of the plot
axis(side = 2, at = seq(1, nrow(three_env$response)) - 0.5, 
     labels = three_env$stimulus_features$animal_names, 
     las = 2)

mtext(text = "Trial", side = 1, outer = TRUE, cex = 1.8, line = 1.5)

dev.off()
