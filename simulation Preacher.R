library("MASS")
library("dplyr")
library("bootnet")
library("psychonetrics")
library("ggplot2")
source("nw function.R")
source("myplot.R")

# ------ Create network matrices ------ #
degree  <- getnw("degree")
close   <- getnw("close")
between <- getnw("between")
alt     <- getnw("alt")

degree2  <- getnw("degree2")
close2   <- getnw("close2")
between2 <- getnw("between2")
alt2     <- getnw("alt2")
# ------------------------------------ #

# ------ Create Preacher simulation ------ #
preacher <- function(models, niter = 1000, nobs = 100){
  
  # Get the dimension of the network matrix
  dim <- nrow(models$full)
  
  # Create empty data frame
  LLs <- data.frame(Full = numeric(),
                    A = numeric(),
                    B = numeric())
  
  for(i in 1:niter){
    # Create positive-definite covariance matrix
    covmat <- crossprod(matrix(runif(dim^2, -1, 1), dim))
    
    # Make correlation matrix from covariance matrix
    # And generate data from that matrix with means of 0.
    # cormat <- cov2cor(covmat)
    # dat <- mvrnorm(nobs, rep(0, dim), cormat)

    # Fit the models on the data
    # mod_full  <- ggm(data = dat, omega = models$full) %>% runmodel
    # mod_a     <- ggm(data = dat, omega = models$a) %>% runmodel
    # mod_b     <- ggm(data = dat, omega = models$b) %>% runmodel
    
    # Fit the models on the covariance matrix
    mod_full  <- ggm(covs = covmat, nobs = nobs, covtype = "ML", omega = models$full) %>% runmodel
    mod_a     <- ggm(covs = covmat, nobs = nobs, covtype = "ML", omega = models$a) %>% runmodel
    mod_b     <- ggm(covs = covmat, nobs = nobs, covtype = "ML", omega = models$b) %>% runmodel

    # Fill the data frame with the LL values
    LLs[i, "Full"]  <- fit(mod_full)[1, 2]
    LLs[i, "A"]     <- fit(mod_a)[1, 2]
    LLs[i, "B"]     <- fit(mod_b)[1, 2]

  }
  
  # Make data frame long format for ggplot
  LLs <- tidyr::gather(LLs, Model, Fit)
  return(LLs)
  
}

# ------ Run Preacher method ------ #
LL_degree  <- preacher(models = degree)
LL_close   <- preacher(models = close)
LL_between <- preacher(models = between)
LL_alt     <- preacher(models = alt)

LL_degree2  <- preacher(models = degree2)
LL_close2   <- preacher(models = close2)
LL_between2 <- preacher(models = between2)
LL_alt2     <- preacher(models = alt2)
# ------------------------------------ #

# ------ Plot ECDFs ------ #
myplot(LL_degree)
myplot(LL_close)
myplot(LL_between)
myplot(LL_alt)

myplot(LL_degree2)
myplot(LL_close2)
myplot(LL_between2)
myplot(LL_alt2)
# ------------------------------------ #
