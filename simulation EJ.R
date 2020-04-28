### EJ
degree_half  <- getnw("degree", half = T)
close_half   <- getnw("close", half = T)
between_half <- getnw("between", half = T)
alt_half     <- getnw("alt", half = T)

nedges_degree   <- 11-1
nedges_close    <- 12-1
nedges_between  <- 8-1
nedges_alt      <- 8-1

mimicry <- function(models, nedges, niter = 1000, nobs = 100){
  
  fitdif <- data.frame(a_b = numeric(),
                       b_a = numeric())

  for(i in 1:niter){
    edgevals <- runif(nedges, 0.1, .5)
    
    models$a[which(models$a==1)] <- edgevals
    models$a[lower.tri(models$a)] <- t(models$a)[lower.tri(models$a)]
    
    models$b[which(models$b==1)] <- edgevals
    models$b[lower.tri(models$b)] <- t(models$b)[lower.tri(models$b)]
    
    data_a <- ggmGenerator()(nobs, models$a)
    data_b <- ggmGenerator()(nobs, models$b)
    
    fit_a_a <- ggm(data_a, omega = models$a) %>% runmodel
    fit_a_b <- ggm(data_a, omega = models$b) %>% runmodel
    
    fitdif[i, "a_b"] <- fit(fit_a_a)[1, 2] - fit(fit_a_b)[1, 2]
    
    fit_b_a <- ggm(data_b, omega = models$a) %>% runmodel
    fit_b_b <- ggm(data_b, omega = models$b) %>% runmodel
    
    fitdif[i, "b_a"] <- fit(fit_b_b)[1, 2] - fit(fit_b_a)[1, 2]
  }
  
  return(fitdif)
  
}

fitdif_degree <- mimicry(models = degree_half, nedges = nedges_degree)
fitdif_close  <- mimicry(models = close_half, nedges = nedges_close)
fitdif_between  <- mimicry(models = between_half, nedges = nedges_between)
fitdif_alt  <- mimicry(models = alt_half, nedges = nedges_alt)



hista_degree <- hist(fitdif_degree$a_b)
histb_degree <- hist(fitdif_degree$b_a)
xlim <- c(min(fitdif_degree$a_b, fitdif_degree$b_a), max(fitdif_degree$a_b, fitdif_degree$b_a))
plot(hista_degree, col=rgb(1,0,0,.5), xlim = xlim)
plot(histb_degree, col=rgb(0,0,1,.5), add=T)


hista_close <- hist(fitdif_close$a_b)
histb_close <- hist(fitdif_close$b_a)
xlim <- c(min(fitdif_close$a_b, fitdif_close$b_a), max(fitdif_close$a_b, fitdif_close$b_a))
plot(hista_close, col=rgb(1,0,0,.5), xlim=xlim)
plot(histb_close, col=rgb(0,0,1,.5), xlim=xlim, add=T)

hista_between <- hist(fitdif_between$a_b)
histb_between <- hist(fitdif_between$b_a)
xlim <- c(min(fitdif_between$a_b, fitdif_between$b_a), max(fitdif_between$a_b, fitdif_between$b_a))
plot(hista_between, col=rgb(1,0,0,.5), xlim=xlim)
plot(histb_between, col=rgb(0,0,1,.5), xlim=xlim, add=T)

hista_alt <- hist(fitdif_alt$a_b)
histb_alt <- hist(fitdif_alt$b_a)
xlim <- c(min(fitdif_alt$a_b, fitdif_alt$b_a), max(fitdif_alt$a_b, fitdif_alt$b_a))
plot(hista_alt, col=rgb(1,0,0,.5), xlim=xlim)
plot(histb_alt, col=rgb(0,0,1,.5), xlim=xlim, add=T)

