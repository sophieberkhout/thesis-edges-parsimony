myplot <- function(df){
  ggplot(df, aes(x = fit, colour = model)) + 
    stat_ecdf() + 
    theme_classic() +
    ylab("")
}
