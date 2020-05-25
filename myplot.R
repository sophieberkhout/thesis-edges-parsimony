myplot <- function(df, title = "", type = "preacher"){
  if(type == "preacher"){
    cols <- c("red", "blue", "black")
    xlab <- "LL"
    labs <- c("A", "B", "Full")
  }
  
  if(type == "mimicry"){
    cols <- c("blue", "red")
    xlab <- expression(Delta*LL)
    labs <- expression(B[a]-A[a], A[b]-B[b])
  }
   
  ggplot(df, aes(x = Fit, colour = Model)) + 
    stat_ecdf(size = 1.5) + 
    scale_colour_manual(values = cols, labels = labs) +
    theme_classic() +
    ylab("") + 
    xlab(xlab) +
    labs(colour = "") + 
    ggtitle(title) + 
    theme(text = element_text(size = 30, family = "serif"),
          axis.ticks.length = unit(-0.25, "cm"), 
          axis.ticks = element_line(size = 1.25),
          axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
          axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
          axis.line = element_line(size = 1.25),
          legend.position = c(.15, .85),
          plot.margin = margin(2, 50, 10, 1))
}


