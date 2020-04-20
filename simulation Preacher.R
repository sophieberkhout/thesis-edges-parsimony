library("MASS")
library("dplyr")
library("bootnet")
library("psychonetrics")

# ------ Create network matrices ------ #

# --- Degree network --- #

nw_degree <- getnw("degree")

degree_a <- nw_degree
degree_a[2, 6] <- degree_a[6, 2] <- 0

degree_b <- nw_degree
degree_b[3, 7] <- degree_b[7,3] <- 0

# --- Closeness network --- #

nw_close <- getnw("close")

close_a <- nw_close
close_a[3, 7] <- close_a[7, 4] <- 0

close_b <- nw_close
close_b[2, 6] <- close_n[6, 2] <- 0

# --- Betweenness network --- #

nw_between <- getnw("between")

between_a <- nw_between
between_a[2, 7] <- between_a[7, 2] <- 0

between_b <- nw_between
between_b[5, 6] <- between_b[6, 5] <- 0

# --- Alternative network --- #
nw_alt <- getnw("alt")

# Model A with edge 1-2 to zero
alt_a <- nw_alt
alt_a[1, 2] <- alt_a[2, 1] <- 0

# Model B with edge 3-5 to zero
alt_b <- nw_alt
alt_b[3, 5] <- alt_b[5, 3] <- 0

# ------------------------------------#

LL_alt_a <- vector()
LL_alt_b <- vector()
for(i in 1000){
  
  # Generate random positive-definite matrix
  covmat <- crossprod(matrix(runif(6^2, -1 ,1), 6))
  
  data <- mvrnorm(100, rep(0, 6), Sigma = covmat)
  
  # Fit models A and B to the matrix
  mod_alt_a <- ggm(covs = covmat, nobs = 100, covtype = "ML", omega = omega_A) %>% runmodel
  mod_alt_b <- ggm(covmat, omega = omega_B) %>% runmodel
  
  # Get LL's from fit
  LL_alt_a[i] <- fit(mod_alt_a)[1, 2]
  LL_alt_b[i] <- fit(mod_alt_b)[1, 2]
  
}











# mod_alt_data <- ggm(data4, omega = omega_alt_a) %>% runmodel
# fit(mod_alt_data)
# 
# ll4_A <- vector()
# ll4_B <- vector()
# for(i in 1:30){
#   covmat <- crossprod(matrix(runif(6^2, -1 ,1), 6))
#   
#   mod4_A <- ggm(covmat, omega = omega_A) %>% runmodel
#   mod4_B <- ggm(covmat, omega = omega_B) %>% runmodel
#   
#   ll4_A[i] <- fit(mod4_A)[1, 2]
#   ll4_B[i] <- fit(mod4_B)[1, 2]
# }
# 
# 
# ll4_A <- ll4_A[order(ll4_A)]
# plot(ll4_A, cumsum(ll4_A)/sum(ll4_A), type = "l")
# ll4_B <- ll4_B[order(ll4_B)]
# lines(ll4_B, cumsum(ll4_B)/sum(ll4_B), col = "red")
# 
# 
# plot(ll4_A, cumsum(ll4_A)/sum(ll4_A))
# lines(ecdf(ll4_A))

t1 <- ggm(covs = covmat, nobs = 100, covtype = "ML", omega = omega_A) %>% runmodel
t2 <- ggm(covs = covmat, nobs = 20, covtype = "ML", omega = omega_A) %>% runmodel
t3 <- ggm(covs = covmat, nobs = 500, covtype = "ML", omega = omega_A) %>% runmodel
ft1 <- fit(t1)[1, 2]
ft2 <- fit(t2)[1, 2]
ft3 <- fit(t3)[1, 2]

k1 <- ggm(covs = covmat, nobs = 100, covtype = "ML", omega = omega_B) %>% runmodel
k2 <- ggm(covs = covmat, nobs = 20, covtype = "ML", omega = omega_B) %>% runmodel
k3 <- ggm(covs = covmat, nobs = 500, covtype = "ML", omega = omega_B) %>% runmodel
fk1 <- fit(k1)[1, 2]
fk2 <- fit(k2)[1, 2]
fk3 <- fit(k3)[1, 2]

