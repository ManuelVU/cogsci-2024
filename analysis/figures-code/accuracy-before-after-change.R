
three_environments <- 
  readr::read_csv(file = paste(c("data/csv-files/",
                                 "three-environments-Disease-filtered-2-more-",
                                 "categories.csv"), collapse = ""))

pdf(file = "figures/accuracy-before-after-change.pdf", width = 7, height = 5)

par(oma = c(2, 4, 0.05, 0.05),
    mai = c(0.1,0.47,0.1,0.1))

source(file = "src/plot-f/plot-before-after.R")

color_cat <- c("#64c5eb", "#7f58af", "#e84d8a", "#feb326")

plot.before.after(data = three_environments, domain = "Disease", 
                  trials_before = 18, trials_after = 18, 
                  color_combination = color_cat)

mtext(text = "Accuracy", side = 2, line = 2.9, cex = 1.2)

mtext(text = "Trial relative to category change", side = 1, cex = 1.2, 
      line = 2.8)


dev.off()
