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
preacher <- function(models, niter = 1000, nobs = 100, mincov = -1, maxcov = 1){
  
  # Get the dimension of the network matrix
  dim <- nrow(models$full)
  
  # Create empty data frame
  LLs <- data.frame(Full = numeric(),
                    A = numeric(),
                    B = numeric())
  
  for(i in 1:niter){
    # Create positive-definite covariance matrix
    covmat <- crossprod(matrix(runif(dim^2, mincov, maxcov), dim))
    
    err <- try({
      # Fit the models on the covariance matrix
    mod_full  <- ggm(covs = covmat, nobs = nobs, covtype = "ML", omega = models$full) %>% runmodel
    mod_a     <- ggm(covs = covmat, nobs = nobs, covtype = "ML", omega = models$a) %>% runmodel
    mod_b     <- ggm(covs = covmat, nobs = nobs, covtype = "ML", omega = models$b) %>% runmodel

    # Fill the data frame with the LL values
    LLs[i, "Full"]  <- fit(mod_full)[1, 2]
    LLs[i, "A"]     <- fit(mod_a)[1, 2]
    LLs[i, "B"]     <- fit(mod_b)[1, 2]
    })
    
    if(class(err) == "try-error") next
    
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

# ------------- Plot ECDFs ----------- #

myplot(LL_degree)
myplot(LL_close)
myplot(LL_between)
myplot(LL_alt)

myplot(LL_degree2)
myplot(LL_close2)
myplot(LL_between2)
myplot(LL_alt2)

# ------------------------------------ #

# -------- LL means and sds ---------- #

# M and SD for model type degree Model A
colMeans(subset(LL_degree2, Model == "A", Fit))
apply(subset(LL_degree2, Model == "A", Fit), 2, sd)

# M and SD for model type degree Model B
colMeans(subset(LL_degree2, Model == "B", Fit))
apply(subset(LL_degree2, Model == "B", Fit), 2, sd)

# Conduct t-test if difference is significant for model type degree
t.test(subset(LL_degree2, Model == "A", Fit)$Fit, 
       subset(LL_degree2, Model == "B", Fit)$Fit, 
       paired = T, alternative = "two.sided")

# See how many LL values of A are larger than B for model type degree
sum(subset(LL_degree2, Model == "A", Fit) > subset(LL_degree2, Model == "B", Fit))


# Do the same for model type closeness 
colMeans(subset(LL_close2, Model == "A", Fit))
apply(subset(LL_close2, Model == "A", Fit), 2, sd)

colMeans(subset(LL_close2, Model == "B", Fit))
apply(subset(LL_close2, Model == "B", Fit), 2, sd)

t.test(subset(LL_close2, Model == "A", Fit)$Fit, 
       subset(LL_close2, Model == "B", Fit)$Fit, 
       paired = T, alternative = "two.sided")

sum(subset(LL_close2, Model == "A", Fit) > subset(LL_close2, Model == "B", Fit))

# Do the same for model type betweenness
colMeans(subset(LL_between2, Model == "A", Fit))
apply(subset(LL_between2, Model == "A", Fit), 2, sd)

colMeans(subset(LL_between2, Model == "B", Fit))
apply(subset(LL_between2, Model == "B", Fit), 2, sd)

t.test(subset(LL_between2, Model == "A", Fit)$Fit, 
       subset(LL_between2, Model == "B", Fit)$Fit, 
       paired = T, alternative = "two.sided")

sum(subset(LL_between2, Model == "A", Fit) > subset(LL_between2, Model == "B", Fit))


# Do the same for model type alternative paths
colMeans(subset(LL_alt2, Model == "A", Fit))
apply(subset(LL_alt2, Model == "A", Fit), 2, sd)

colMeans(subset(LL_alt2, Model == "B", Fit))
apply(subset(LL_alt2, Model == "B", Fit), 2, sd)

t.test(subset(LL_alt2, Model == "A", Fit)$Fit, 
       subset(LL_alt2, Model == "B", Fit)$Fit, 
       paired = T, alternative = "two.sided")

sum(subset(LL_alt2, Model == "A", Fit) > subset(LL_alt2, Model == "B", Fit))

# ------------------------------------ #
