################################################################################
# Plot average filtered accuracy before and after a change
################################################################################

plot.before.after <- function (data, domain, trials_before, 
                               trials_after = trials_before, 
                               x_values = c(-trials_before, trials_after),
                               y_values = c(0,1),
                               color_positive = "#62929E", 
                               color_negative = "#393D3f", 
                               color_combination = c("#4DB3E4","#B83C70",
                                                     "#20CC77","#FDA649"),
                               x_ann = TRUE, y_ann = TRUE,
                               before_ltype = "l", after_ltype = "o", 
                               pch_plt = seq(15,18,1), lwd = 1.5, 
                               axis_lwd = 1.6, cex_axis = 1.6, padj = 0, 
                               hadj = 0.9, legend_add = TRUE, 
                               legend_pos = c(x_values[1], 0.44), cex.leg = 1.2,
                               conditional_lty = c(1,2)) {
  
  source(file = "src/data-f/before-after-stimulus.R")
  source(file = "src/data-f/column-means.R")
  source(file = "src/data-f/accuracy-before.R")
  source(file = "src/data-f/accuracy-after-value.R")
  source(file = "src/data-f/accuracy-after-value-part.R")
  
  plot(x = 0, y = 0, ann = FALSE, axes = FALSE, type = "n", xlim = x_values, 
       ylim = y_values)
  
  box(bty = "l")
  
  if (x_ann == TRUE) {
  
    axis(side = 1, at = c(x_values[1], floor(x_values[1]/2), - 1),
         lwd = axis_lwd, cex.axis = cex_axis, padj = padj, hadj = 0.7)
    
    axis(side = 1, at = c(1, ceiling(x_values[2]/2), x_values[2]),
         lwd = axis_lwd, cex.axis = cex_axis, padj = padj)
    
  }
  else {
    
    axis(side = 1, at = c(x_values[1], floor(x_values[1]/2), 
                          - 1, 1, 
                          ceiling(x_values[2]/2), x_values[2]), 
         labels = rep("",6), lwd = axis_lwd)
    
  }
  if (y_ann == TRUE) {
    
    axis(side = 2, at = c(0,0.5,1), labels = c("0","0.5","1"), las = 2, 
         lwd = axis_lwd, cex.axis = cex_axis, hadj = hadj)
    
  }
  else {
    
    axis(side = 2, at = c(0,0.5,1), labels = rep("",3), las = 2, lwd = axis_lwd)
    
  }
  
  positive_line <- column.means(
    accuracy.before(data = data, domain = domain, value = "positive",
                    trials_before = trials_before))
  
  negative_line <- column.means(
    accuracy.before(data = data, domain = domain, value = "negative",
                    trials_before = trials_before))
  
  lines(x = seq(x_values[1], -1), y = positive_line, type = before_ltype, 
        col = color_positive, lwd = lwd, lty = conditional_lty[1])
  
  lines(x = seq(x_values[1], -1), y = negative_line, type = before_ltype, 
        col = color_negative, lwd = lwd, lty = conditional_lty[2])
  
  abline(v = 0, lwd = 1.4, col = "slategray", lty = 3)
  
  combinations <- cbind(rep(x = c("positive", "negative"), each = 2),
                        rep(x = c("positive", "negative"), times = 2))
  
  for(cc in 1:dim(combinations)[1]){
    line_new <- column.means(
      accuracy.after.value(data = data, domain = domain, 
                           trials_before = trials_before, 
                           trials_after = trials_after, 
                           value_before = combinations[cc, 1],
                           value_after = combinations[cc, 2]))
    
    lines(x = seq(1, x_values[2]), y = line_new, type = after_ltype, 
          pch = pch_plt[cc], col = color_combination[cc], lwd = lwd, 
          lty = conditional_lty[(combinations[cc,1] == "negative") + 1])
    
  }
  if(legend_add == TRUE){
    
    d <- domain
    
    q <- subset(x = data, subset = domain_char == d) 
    
    positive_label <- unique(q$category_char[q$category == 0])
    
    negative_label <- unique(q$category_char[q$category == 1])
    
    legend(x = legend_pos[1], y = legend_pos[2],  
           legend = c(positive_label, 
                      negative_label,
                      paste(c(positive_label, " to ", positive_label), 
                            collapse = ""),
                      paste(c(positive_label, " to ", negative_label),
                            collapse = ""),
                      paste(c(negative_label, " to ", positive_label),
                            collapse = ""),
                      paste(c(negative_label, " to ", negative_label),
                            collapse = "")),
           col = c(color_positive, color_negative, color_combination),
           lty = 1, pch = c(rep(x = NA, times = 2), pch_plt), bty = "n",
           cex = cex.leg, lwd = 1.5, pt.cex = 1.3)
  }
}