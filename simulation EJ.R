library("MASS")
library("dplyr")
library("bootnet")
library("psychonetrics")
library("ggplot2")
source("nw function.R")
source("myplot.R")
library("corpcor")
library("purrr")

# ------ Create (half) network matrices ------ #

degree_half  <- getnw("degree", half = T)
close_half   <- getnw("close", half = T)
between_half <- getnw("between", half = T)
alt_half     <- getnw("alt", half = T)

degree2_half  <- getnw("degree2", half = T)
close2_half   <- getnw("close2", half = T)
between2_half <- getnw("between2", half = T)
alt2_half     <- getnw("alt2", half = T)

# ------------------------------------------- #

# ------ Number of edges in models ------ #

nedges_degree   <- 11-1
nedges_close    <- 9-1
nedges_between  <- 8-1
nedges_alt      <- 8-1

nedges_degree2   <- 8-1
nedges_close2    <- 9-1
nedges_between2  <- 9-1
nedges_alt2      <- 8-1

# --------------------------------------- #

# ------ Create model mimicry simulation ------ #

mimicry <- function(models, nedges, niter = 1000, nobs = 100, test = F){
  
  # Create empty data frame
  fitsep <- data.frame(Aa = numeric(),
                       Ba = numeric(),
                       Ab = numeric(),
                       Bb = numeric())
  
  fitdif <- data.frame(Ba_Aa = numeric(),
                       Ab_Bb = numeric())
  
  # Get the number of variables in the models
  dim <- nrow(models$full)
  
  if(test) edge_values <- data.frame(edgevals = numeric())
  
  # Get the location of the edges in the network matrix
  # The edge that is different for the models is placed last
  loc_a <- c(which(models$a==1 & models$a==models$b), 
             which(models$a==1 & models$a!=models$b))
  
  loc_b <- c(which(models$b==1 & models$b==models$a),
             which(models$b==1 & models$b!=models$a))
  
  # Create the basis for the network models
  pcor_a <- models$a
  pcor_b <- models$b
  
  # Make the models symmetrical 
  models$a[lower.tri(models$a)] <- t(models$a)[lower.tri(models$a)]
  models$b[lower.tri(models$b)] <- t(models$b)[lower.tri(models$b)]
  
  
  for(i in 1:niter){

    # Generate edge weights
    edgevals <- runif(nedges, .1, .4)

    # Fill partial correlation matrix for Model A with the edge weights
    pcor_a[loc_a] <- edgevals
    
    # Make the matrix symmetrical
    pcor_a[lower.tri(pcor_a)] <- t(pcor_a)[lower.tri(pcor_a)]
    
    # Add ones on the diagonal
    diag(pcor_a) <- 1
    
    # Convert partical correlation matrix into correlation matrix
    cor_a <- pcor2cor(pcor_a)
    
    # Do the some for model B
    pcor_b[loc_b] <- edgevals
    pcor_b[lower.tri(pcor_b)] <- t(pcor_b)[lower.tri(pcor_b)]
    diag(pcor_b) <- 1
    cor_b <- pcor2cor(pcor_b)
    
    err <- try({
      # Generate data from both networks
        data_a <- mvrnorm(nobs, rep(0, dim), cor_a)
        data_b <- mvrnorm(nobs, rep(0, dim), cor_b)
      
        # Fit both models on data from model a
        fit_Aa <- ggm(data_a, omega = models$a) %>% runmodel
        fit_Ba <- ggm(data_a, omega = models$b) %>% runmodel
        
        # Get the difference in fit for data_a (model b - model a) (just LL values of both)
        fitsep[i, "Aa"] <- fit(fit_Aa)[1, 2]
        fitsep[i, "Ba"] <- fit(fit_Ba)[1, 2]
        fitdif[i, "Ba_Aa"] <- fitsep[i, "Ba"] - fitsep[i, "Aa"]
        
        # Fit both models on data from model b
        fit_Ab <- ggm(data_b, omega = models$a) %>% runmodel
        fit_Bb <- ggm(data_b, omega = models$b) %>% runmodel
        
        # Get the difference in fit for data_b (model a - model b)
        fitsep[i, "Bb"] <- fit(fit_Bb)[1, 2]
        fitsep[i, "Ab"] <- fit(fit_Ab)[1, 2] 
        fitdif[i, "Ab_Bb"] <- fitsep[i, "Ab"] - fitsep[i, "Bb"]
        
    })
    

    if(class(err) == "try-error") next
      
    if(test){
      edge_values <- rbind(edge_values, edgevals)
    }
      
  }
    

  if(test){
    allresults <- list(fitsep = fitsep,
                       edge_values = edge_values)
    
    return(allresults)
  } else {
    # Make data frame long format for ggplot
    fitdif <- tidyr::gather(fitdif, Model, Fit)
    
    return(fitdif)
  }
}

# ---------------------------------------------- #

# ------ Run mimicry method ------ #

fitdif_degree   <- mimicry(models = degree_half, 
                           nedges = nedges_degree)

fitdif_close    <- mimicry(models = close_half,
                           nedges = nedges_close)

fitdif_between  <- mimicry(models = between_half,
                           nedges = nedges_between)

fitdif_alt      <- mimicry(models = alt_half, 
                           nedges = nedges_alt)


fitdif_degree2  <- mimicry(models = degree2_half, 
                           nedges = nedges_degree2)

fitdif_close2   <- mimicry(models = close2_half, 
                           nedges = nedges_close2)

fitdif_between2 <- mimicry(models = between2_half, 
                           nedges = nedges_between2)

fitdif_alt2     <- mimicry(models = alt2_half, 
                           nedges = nedges_alt2)

# -------------------------------- #

# ------- Plot ECDFs of the differences ------ #

myplot(fitdif_degree, type = "mimicry")
myplot(fitdif_close, type = "mimicry")
myplot(fitdif_between, type = "mimicry")
myplot(fitdif_alt, type = "mimicry")

myplot(fitdif_degree2, type = "mimicry")
myplot(fitdif_close2, type = "mimicry")
myplot(fitdif_between2, type = "mimicry")
myplot(fitdif_alt2, type = "mimicry")

# ------------------------------------------- #

# Calculate means and sds for model type degree
colMeans(subset(fitdif_degree2, Model == "Ab_Bb", Fit))
colMeans(subset(fitdif_degree2, Model == "Ba_Aa", Fit))

apply(subset(fitdif_degree2, Model == "Ab_Bb", Fit), 2, sd)
apply(subset(fitdif_degree2, Model == "Ba_Aa", Fit), 2, sd)

# Conduct t-test for significance
t.test(subset(fitdif_degree2, Model == "Ab_Bb", Fit)$Fit, 
       subset(fitdif_degree2, Model == "Ba_Aa", Fit)$Fit, 
       paired = T, alternative = "two.sided")

# See how many are larger than the other
sum(subset(fitdif_degree2, Model == "Ab_Bb", Fit) > subset(fitdif_degree2, Model == "Ba_Aa", Fit))


# Do the same for model type closeness
colMeans(subset(fitdif_close2, Model == "Ab_Bb", Fit))
colMeans(subset(fitdif_close2, Model == "Ba_Aa", Fit))

apply(subset(fitdif_close2, Model == "Ab_Bb", Fit), 2, sd)
apply(subset(fitdif_close2, Model == "Ba_Aa", Fit), 2, sd)


t.test(subset(fitdif_close2, Model == "Ab_Bb", Fit)$Fit, 
       subset(fitdif_close2, Model == "Ba_Aa", Fit)$Fit, 
       paired = T, alternative = "two.sided")

sum(subset(fitdif_close2, Model == "Ab_Bb", Fit) > subset(fitdif_close2, Model == "Ba_Aa", Fit))

# Do the same for model type between
difbetween <- getdif(fitdif_between2)
colMeans(subset(fitdif_between2, Model == "Ab_Bb", Fit))
colMeans(subset(fitdif_between2, Model == "Ba_Aa", Fit))

apply(subset(fitdif_between2, Model == "Ab_Bb", Fit), 2, sd)
apply(subset(fitdif_between2, Model == "Ba_Aa", Fit), 2, sd)

t.test(subset(fitdif_between2, Model == "Ab_Bb", Fit)$Fit, 
       subset(fitdif_between2, Model == "Ba_Aa", Fit)$Fit, 
       paired = T, alternative = "two.sided")

sum(subset(fitdif_between2, Model == "Ab_Bb", Fit) > subset(fitdif_between2, Model == "Ba_Aa", Fit))

# Do the same for model type alternatives
colMeans(subset(fitdif_alt2, Model == "Ab_Bb", Fit))
colMeans(subset(fitdif_alt2, Model == "Ba_Aa", Fit))

apply(subset(fitdif_alt2, Model == "Ab_Bb", Fit), 2, sd)
apply(subset(fitdif_alt2, Model == "Ba_Aa", Fit), 2, sd)

t.test(subset(fitdif_alt2, Model == "Ab_Bb", Fit)$Fit, 
       subset(fitdif_alt2, Model == "Ba_Aa", Fit)$Fit, 
       paired = T, alternative = "two.sided")

sum(subset(fitdif_alt2, Model == "Ab_Bb", Fit) > subset(fitdif_alt2, Model == "Ba_Aa", Fit))


# ----------------------------------------------- #

# Create seperate ECDFs for all for fits model type degree
myplot_ecdf <- function(dat){
  dat$True <- rep(c("a", "b"), each = 2000)
  xlim <- c(min(dat$Fit), max(dat$Fit))
  dat_a <- subset(dat, True == "a")
  plot_a <- ggplot(dat_a) + stat_ecdf(aes(x = Fit, colour = Model), size = 1.5) +
    scale_colour_manual(values = c("blue", "red"), labels = c(expression(Fit[Aa]), expression(Fit[Ba]))) + 
    theme_classic() +    
    theme(text = element_text(size = 30, family = "serif"),
          axis.ticks.length = unit(-0.25, "cm"), 
          axis.ticks = element_line(size = 1.25),
          axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
          axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
          axis.line = element_line(size = 1.25),
          legend.position = c(.15, .85),
          plot.margin = margin(15, 50, 10, 1),
          legend.title=element_blank()) +
    ylab("") +
    xlab("LL") +
    # ggtitle(expression(Data[a])) +
    xlim(xlim)
  
  dat_b <- subset(dat, True == "b")
  plot_b <- ggplot(dat_b) + stat_ecdf(aes(x = Fit, colour = Model), size = 1.5) +
    scale_colour_manual(values = c("blue", "red"), labels = c(expression(Fit[Ab]), expression(Fit[Bb]))) + 
    theme_classic() +    
    theme(text = element_text(size = 30, family = "serif"),
          axis.ticks.length = unit(-0.25, "cm"), 
          axis.ticks = element_line(size = 1.25),
          axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
          axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
          axis.line = element_line(size = 1.25),
          legend.position = c(.15, .85),
         plot.margin = margin(15, 50, 10, 1),
         legend.title=element_blank()) +
    ylab("") +
    xlab("LL") +
    # ggtitle(expression(Data[b])) +
    xlim(xlim)
  
  plots <- list(plot_a = plot_a, 
                plot_b = plot_b)
  return(plots)
}

sepdif_degree2   <- mimicry(models = degree2_half,
                            nedges = nedges_degree2,
                            test = T) 
sep_ecdfs_degree <- myplot_ecdf(sepdif_degree2)
# sep_ecdfs_degree$plot_a
# sep_ecdfs_degree$plot_b

# If test = T, this function can calculate the differences
getdif <- function(dat){
  dat <- tidyr::gather(dat, Model, Fit)
  difBaAa <- subset(dat, Model == "Ba", Fit) - subset(dat, Model == "Aa", Fit)
  difAbBb <- subset(dat, Model == "Ab", Fit) - subset(dat, Model == "Bb", Fit)
  difs <- data.frame(BaAa = difBaAa, AbBb = difAbBb)
  names(difs) <-c("Ba_Aa", "Ab_Bb")
  difs <- tidyr::gather(difs, Model, Fit)
  return(difs)
}
