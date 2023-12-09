# load plotting function
source(file = "src/plot-f/plot-trial-trial.R")

# load data transformation function
source(file = "src/sampling-f/three-parameters/transform-data-chmm.R")

# load block samples function
source(file = "src/data-f/join-block-samples.R")

# load data in chmm format
three_env <- transform_data_chmm(
  directory_data = 
    paste(c("data/csv-files/",
            "three-environments-Disease-filtered-block-sampling.csv"), 
          collapse = ""),
  directory_features = "data/stimuli-features/fast_1_2_similarity_matrix.csv",
  similarity = TRUE)

# load data in long format to find changepoints
tmp <- readr::read_csv(
  file = paste(c("data/csv-files/",
                 "three-environments-Disease-filtered-block-sampling.csv"), 
               collapse = ""))

# change stimulus order and plot by similarity
stimulus_order <- c(20, 15, 10, 11, 3, 21, 1, 8, 14, 7,
                    12, 19, 9, 4, 17, 18, 2, 6, 16, 5, 13)

# change points by participant
cp_1 <- unique(subset(x = tmp,
                      subset = change_point == TRUE & id == 2)$trial_domain)

cp_2 <- unique(subset(x = tmp,
                      subset = change_point == TRUE & id == 16)$trial_domain)

# set participant id numbers in terms of the dimention of the samples
participants <- c(1,2)

# join bolck samples
samples <- block_posteriors(participant_id = participants,
                            start_points = rbind(c(1, cp_1[-length(cp_1)]),
                                                 c(1, cp_2[-length(cp_2)])),
                            end_points = rbind(c(cp_1 - 1),
                                               c(cp_2 - 1)),
                            parameters = 3, 
                            similarity = TRUE)


# color by category
color_cat <- c("#64c5eb", "#7f58af", "#e84d8a", "#feb326")

grDevices::cairo_pdf(
  file = paste(c("figures/",
                 "trial-trial-plot-three-env-all-states-3-parameters.pdf"), 
               collapse = ""),
  width = 8.3, height = 8, onefile = TRUE)

# set participant id to plot
part <- unique(tmp$id)

for (pp in 1:length(part)) {
  layout(rbind(c(1, 1, 1, 1, 1),
               c(1, 1, 1, 1, 1),
               c(1, 1, 1, 1, 1),
               c(1, 1, 1, 1, 1),
               c(1, 1, 1, 1, 1),
               c(2, 3, 4, 5, 6),
               c(2, 3, 4, 5, 6),
               c(2, 3, 4, 5, 6)))
  
  par(oma = c(1.5, 4.7, 2.2, 1),
      mai = c(0.4, 0.5, 0.1, 0),
      yaxs = "i", 
      xaxs = "i")
  
  # find change points of participant pp
  cp <- which(x = subset(x = tmp, subset = id == part[pp])$change_point == TRUE)
  
  # transform condition into numeric ordered variable
  condition_ordered <- sort(
    unique(tmp$condition_char[which(tmp$id == part[pp])]))
  
  cc <- ifelse(
    test = tmp$condition_char[which(tmp$id == part[pp])] %in%
      condition_ordered[1],
    yes = 1, 
    no = ifelse(
      test = tmp$condition_char[which(tmp$id == part[pp])] %in%
        condition_ordered[2],
      yes = 2, 
      no = ifelse(test = tmp$condition_char[which(tmp$id == part[pp])] %in%
                    condition_ordered[3],
                  yes = 3,
                  no = 4)))
  
  truth_value <- subset(x = tmp, 
                        subset = id == part[pp])$category_char == "infected"
  
  
  # plot trial by trial responses and posterior mean in a single graph
  trial_trial_participant(data = three_env,
                          plot_order = stimulus_order,
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
                          lwd_rect = 1.5, change_points = cp - 1, 
                          add_category = TRUE,
                          category_str = truth_value)
  
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
       labels = sort(unique(tmp$stimulus_char))[stimulus_order], las = 2, 
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
  
  hist(samples$posterior_samples$kappa[, pp], xlim = c(0,2), axes = FALSE,
       ann = FALSE, border = "#FCF6F5", col = "#30626d",
       breaks = seq(0, 2,0.1))
  box(bty = "l")
  axis(side = 1, at = c(0, 0.5, 1, 1.5, 2), 
       labels = c("0", "0.5", "1", "1.5", "2"),
       cex.axis = 1.4, padj = -0.2)
  mtext(text = expression(kappa), side = 1, line = 3, cex = 1.6)
  
}

dev.off()

