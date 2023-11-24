################################################################################
# Filtered accuracy plot for all participants in a task
################################################################################

three_environments <- 
  readr::read_csv(file = paste(c("data/csv-files/",
                                 "three-environments-Disease-filtered-2-more-",
                                 "categories.csv"), collapse = ""))

diseases <- c('Brucellosis', 'Echinococcus', 'Heartwater', 'Paratuberculosis')


pdf(file = "figures/filtered-accuracy-all.pdf", width = 7, height = 6)

source(file = "src/plot-f/plot-filtered-accuracy.R")

par(oma = c(2, 2.2, 0.1, 0.1),
    mai = c(0.07, 0.07, 0.07, 0.07))

layout(matrix(data = c(seq(1, 56)), nrow = 8, ncol = 7, byrow = TRUE))

color_cat <- c("#64c5eb", "#7f58af", "#e84d8a", "#feb326")

participant_id <- unique(three_environments$id)

count <- 0
annotation <- 54

for (pt in participant_id) {
  
  count <- count + 1
  
  if (pt != annotation) {
    
    plot.filtered.acc(data = three_environments, participant = pt, 
                      domain = "Disease", color_category = color_cat, 
                      cex_pt = 0.6,
                      ltype = "o", pch_plt = 16, lwd = 1.2, x_ann = FALSE)
    
  } else {
    plot.filtered.acc(data = three_environments, participant = pt, 
                      domain = "Disease", color_category = color_cat, 
                      cex_pt = 0.6,
                      ltype = "o", pch_plt = 16, lwd = 1.2, 
                      x_ann = TRUE, y_ann = TRUE, cex_axis = 1.2, padj = -0.5,
                      hadj = 0.8)
  }
  
  
  legend(x = 70, y = 0.36, bty = "n",
         legend = paste(c("P.", participant_id[count]), collapse = ""),
         cex = 1.18)
  
}

mtext(text = "Accuracy", side = 2, line = 0.6, outer = TRUE, cex = 1.4)
mtext(text = "Trial", side = 1, line = 0.9, outer = TRUE, cex = 1.4)

par(fig = c(0.3,0.6,0,0.1), 
    new = TRUE)

plot(x = 0, y = 0, ann = FALSE, axes = FALSE, type = "n")
legend("left", legend = diseases[c(1, 2)], lty = 1, 
       col = color_cat[c(1, 2)], bty = "n", lwd = 3, cex = 1.3)

par(fig = c(0.6,0.9,0,0.1), 
    new = TRUE)

plot(x = 0, y = 0, ann = FALSE, axes = FALSE, type = "n")
legend("left", legend = diseases[c(3, 4)], lty = 1, 
       col = color_cat[c(3, 4)], bty = "n", lwd = 3, cex = 1.3)

dev.off()
