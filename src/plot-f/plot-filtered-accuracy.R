################################################################################
# Plotting function for filtered accuracy in a fixed domain
################################################################################

# Filtered accuracy plot function ----------------------------------------------
plot.filtered.acc <- function(data, participant, domain, xvalues = c(0,211),
                              x_ann = TRUE, y_ann = FALSE, ltype = "o", 
                              pch_plt = 16, lwd = 1.5,
                              color_category = c("#4DB3E4","#B83C70",
                                                 "#20CC77","#FDA649"),
                              axis_lwd = 1.6, cex_axis = 1.6, padj = 0, 
                              hadj = 0.9, cex_pt = 0.5,
                              window_size = 10, alpha = NA, filter = "ma"){
  
  categories <- sort(unique(data$condition_char[data$domain_char == domain]))
  
  plot(x = 0, y = 0, axes = FALSE, ann = FALSE, xlim = xvalues,
       ylim = c(-0.02,1.02),
       type = 'n')
  
  box(bty = "l")
  
  if(x_ann == TRUE){
    axis(side = 1, at = c(1, 105, 210), labels = c(1, 105, 210), 
         lwd = axis_lwd, cex.axis = cex_axis, padj = padj, tck = -0.07)  
  }
  else{
    axis(side = 1, at = c(1, 105, 210), labels = rep("",3), lwd = axis_lwd,
         tck = -0.07)
  }
  
  if(y_ann == TRUE){
    axis(side = 2, at = c(0,0.5,1), labels = c("0","0.5","1"), las = 2, 
         lwd = axis_lwd, cex.axis = cex_axis, hadj = hadj, tck = -0.07)
  }
  else{
    axis(side = 2, at = c(0,0.5,1), labels = rep("",3), las = 2, lwd = axis_lwd,
         tck = -0.07)
  }
  
  source(file = "src/data-f/pull-participant-domain.R")
  source(file = "src/data-f/filter-changepoint.R")
  
  q <- pull.part.domain(data = data, participant = participant, 
                        domain_keep = domain)
  
  cats <- sort(unique(q$condition_char))
  
  for(ii in cats){
    line_values <- filter.changepoint(data = data, 
                                      participant = participant,
                                      category_keep = ii,
                                      domain_keep = domain, 
                                      filter = filter, 
                                      window_size = window_size, 
                                      alpha = alpha)
    
    if(sum(line_values$change_point) > 0){
      
      t_cp <- c(1, which(line_values$change_point == TRUE), 
                dim(line_values)[1] + 1)
      
      for(tt in 2:length(t_cp)){
        
        lines(x = line_values$trial_domain[t_cp[tt-1]:(t_cp[tt]-1)], 
              y = line_values$filtered_acc[t_cp[tt-1]:(t_cp[tt]-1)], 
              type = ltype, col = color_category[which(categories == ii)],
              pch = pch_plt, lwd = lwd, cex = cex_pt)
        
      }
    }
    else{
      lines(x = line_values$trial_domain, 
            y = line_values$filtered_acc, 
            type = ltype, col = color_category[which(categories == ii)], 
            pch = pch_plt, lwd = lwd, cex = cex_pt) 
      
    }
    
  }
}