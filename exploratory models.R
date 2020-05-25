library("MASS")
library("dplyr")
library("bootnet")
library("psychonetrics")
library("ggplot2")
# source("simulation Preacher.R")
source("myplot.R")



exp1 <- matrix(0, 8, 8)
exp1[1, 2] <- exp1[2, 3] <- 
  exp1[1, 4] <- exp1[2, 5] <- 
  exp1[3, 6] <- exp1[4, 5] <- 
  exp1[5, 6] <- exp1[1, 5] <- 
  exp1[2, 4] <- exp1[2, 6] <- 
  exp1[3, 5] <- exp1[3, 7] <- 
  exp1[6, 8] <- exp1[7, 8] <- 1

exp1_a <- exp1
exp1_a[7, 8] <- 0

exp1_b <- exp1
exp1_b[2, 5] <- 0


exp2 <- matrix(0, 8, 8)
exp2[1, 2] <- exp2[1, 5] <- exp2[1, 6] <- 
  exp2[2, 5] <- exp2[2, 6] <- exp2[3, 4] <- 
  exp2[3, 7] <- exp2[3, 8] <- exp2[4, 7] <- 
  exp2[4, 8] <- exp2[5, 6] <- exp2[6, 7] <- 
  exp2[7, 8] <- 1

exp2_a <- exp2
exp2_a[6, 7] <- 0

exp2_b <- exp2
exp2_b[1, 5] <- 0


exp6 <- matrix(0, 6, 6)
diag(exp6) <- 0

exp6_a <- matrix(0, 6, 6)
exp6_a[1, 2] <- exp6_a[2, 3] <- 
  exp6_a[2, 4] <- exp6_a[2, 5] <- 
  exp6_a[2, 6] <- 1

exp6_b <- matrix(0, 6, 6)
exp6_b[1, 4] <- exp6_b[2, 3] <- 
  exp6_b[3, 6] <- exp6_b[4, 5] <- 
  exp6_b[5, 6] <- 1

makesym <- function(nw, nw_a, nw_b, half = F){
  
  if(!half){
    nw[lower.tri(nw)] <- t(nw)[lower.tri(nw)]
    nw_a[lower.tri(nw_a)] <- t(nw_a)[lower.tri(nw_a)]
    nw_b[lower.tri(nw_b)] <- t(nw_b)[lower.tri(nw_b)]
  }
  
  nwmodels <- list(full = nw,
                   a = nw_a,
                   b = nw_b)
  
  return(nwmodels)
}

nw1 <- makesym(exp1, exp1_a, exp1_b)
nw1_half <- makesym(exp1, exp1_a, exp1_b, half = T)

nw2 <- makesym(exp2, exp2_a, exp2_b)
nw2_half <- makesym(exp2, exp2_a, exp2_b, half = T)


nw6 <- makesym(exp6, exp6_a, exp6_b)
nw6_half <- makesym(exp6, exp6_a, exp6_b, half = T)



LL_nw1 <- preacher(nw1)
fitdif_nw1 <- mimicry(nw1_half, nedges = 14-1)


LL_nw2 <- preacher(nw2)
fitdif_nw2 <- mimicry(nw2_half, nedges = 13-1)

LL_nw6 <- preacher(nw6)
fitdif_nw6 <- mimicry(nw6_half, nedges = 5)


myplot(LL_nw1)
myplot(LL_nw5)
myplot(LL_nw6)

myplot(fitdif_nw1, type = "mimicry")
myplot(fitdif_nw5, type = "mimicry")
myplot(fitdif_nw6, type = "mimicry")
