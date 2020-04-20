### EJ
nw_alt <- matrix(0,6,6)
nw_alt[2, 1] <- nw_alt[4, 1]  <- 
  nw_alt[3, 2]<- nw_alt[5, 2] <-
  nw_alt[5, 3] <- nw_alt[6, 3]  <- 
  nw_alt[5, 4]  <- nw_alt[6, 5]  <- 1

for(i in 1:1000){
  
  edgevals <- runif(7, 0.1, .5)
  
  nw_alt_a <- nw_alt
  nw_alt_a[2, 1] <- 0
  nw_alt_a[which(nw_alt_a==1)] <- edgevals
  nw_alt_a[upper.tri(nw_alt_a)] <- t(nw_alt_a)[upper.tri(nw_alt_a)]
  
  nw_alt_b <- nw_alt
  nw_alt_b[6, 3] <- 0
  nw_alt_b[which(nw_alt_b==1)] <- edgevals
  nw_alt_b[upper.tri(nw_alt_b)] <- t(nw_alt_b)[upper.tri(nw_alt_b)]
}

data_alt_a <- ggmGenerator()(100, nw_alt_a)
data_alt_b <- ggmGenerator()(100, nw_alt_b)

fit_a_a <- ggm(data_alt_a, omega = nw_alt_a) %>% runmodel
fit_a_b <- ggm(data_alt_a, omega = nw_alt_b) %>% runmodel

fit(fit_a_a)[1, 2] - fit(fit_a_b)[1, 2]

fit_b_a <- ggm(data_b, omega = net_alt_a) %>% runmodel
fit_b_b <- ggm(data_b, omega = net_alt_b) %>% runmodel

fit(fit_b_a)[1, 2] - fit(fit_b_b)[1, 2]
