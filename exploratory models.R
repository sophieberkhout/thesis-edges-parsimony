exp1 <- matrix(0, 6, 6)
exp1[1, 2] <- exp1[2, 3] <- 
  exp1[1, 4] <- exp1[2, 5] <- 
  exp1[3, 6] <- exp1[4, 5] <- 
  exp1[5, 6] <- 1

exp1_a <- exp1
exp1_a[2, 5] <- 0

exp1_b <- exp1
exp1_b[1, 4] <- 0

exp2 <- matrix(0, 6, 6)
exp2[1, 2] <- exp2[2, 3] <- 
  exp2[1, 4] <- exp2[2, 5] <- 
  exp2[3, 6] <- exp2[4, 5] <- 
  exp2[5, 6] <- exp2[1, 5] <- 
  exp2[2, 4] <- exp2[2, 6] <- 
  exp2[3, 5] <- 1

exp2_a <- exp2
exp2_a[1, 4] <- 0

exp2_b <- exp2
exp2_b[2, 5] <- 0

exp3 <- matrix(0, 6, 6)
exp3[1, 2] <- 
  exp3[1, 4] <- exp3[2, 5] <- 
  exp3[3, 6] <- exp3[4, 5] <- 
  exp3[1, 5] <- 
  exp3[2, 4] <- exp3[2, 6] <- 
  exp3[3, 5] <- 1

exp3_a <- exp3
exp3_a[2, 6] <- 0

exp3_b <- exp3
exp3_b[1, 4] <- 0

makesym <- function(nw, nw_a, nw_b){
  nw[lower.tri(nw)] <- t(nw)[lower.tri(nw)]
  nw_a[lower.tri(nw_a)] <- t(nw_a)[lower.tri(nw_a)]
  nw_b[lower.tri(nw_b)] <- t(nw_b)[lower.tri(nw_b)]
  
  nwmodels <- list(full = nw,
                   a = nw_a,
                   b = nw_b)
  
  return(nwmodels)
}

nw1 <- makesym(exp1, exp1_a, exp1_b)
nw2 <- makesym(exp2, exp2_a, exp2_b)
nw3 <- makesym(exp3, exp3_a, exp3_b)

LL_nw1 <- preacher(nw1)
LL_nw2 <- preacher(nw2)
LL_nw3 <- preacher(nw3)
