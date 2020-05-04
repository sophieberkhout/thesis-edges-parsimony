library("MASS")
library("dplyr")
library("bootnet")
library("psychonetrics")
library("ggplot2")
source("nw function.R")
source("myplot.R")

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
nedges_alt2      <- 9-1
# --------------------------------------- #

# ------ Create model mimicry simulation ------ #
mimicry <- function(models, nedges, niter = 1000, nobs = 100){
  
  # Create empty data frame
  fitdif <- data.frame(a_b = numeric(),
                       b_a = numeric())

  for(i in 1:niter){
    
    # Get random edge values between .1 and .5
    edgevals <- runif(nedges, .1, .5) 
    
    # Get the location of the edges in the network matrix
    # The edge that is different for the models is placed last
    loc_a <- c(which(models$a==1 & models$a==models$b), 
               which(models$a==1 & models$a!=models$b))
    
    loc_b <- c(which(models$b==1 & models$b==models$a),
               which(models$b==1 & models$b!=models$a))
    
    # Fill model a with the edge values and mirror the matrix
    models$a[loc_a] <- edgevals
    models$a[lower.tri(models$a)] <- t(models$a)[lower.tri(models$a)]
    
    # Fill model b with the edge values and mirror the matrix
    models$b[loc_b] <- edgevals
    models$b[lower.tri(models$b)] <- t(models$b)[lower.tri(models$b)]
    
    # Generate data from both models
    data_a <- ggmGenerator()(nobs, models$a)
    data_b <- ggmGenerator()(nobs, models$b)
    
    # Fit both models on data from model a
    fit_a_a <- ggm(data_a, omega = models$a) %>% runmodel
    fit_a_b <- ggm(data_a, omega = models$b) %>% runmodel
    
    # Get the difference in fit (model a - model b)
    fitdif[i, "a_b"] <- fit(fit_a_a)[1, 2] - fit(fit_a_b)[1, 2]
    
    # Fit both models on data from model b
    fit_b_a <- ggm(data_b, omega = models$a) %>% runmodel
    fit_b_b <- ggm(data_b, omega = models$b) %>% runmodel
    
    # Get the difference in fit (model b - model a)
    fitdif[i, "b_a"] <- fit(fit_b_b)[1, 2] - fit(fit_b_a)[1, 2]
  }
  
  # Make data frame long format for ggplot
  fitdif <- tidyr::gather(fitdif, model, fit)
  
  return(fitdif)
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
myplot(fitdif_degree)
myplot(fitdif_close)
myplot(fitdif_between)
myplot(fitdif_alt)

myplot(fitdif_degree2)
myplot(fitdif_close2)
myplot(fitdif_between2)
myplot(fitdif_alt2)
# ------------------------------------------- #
