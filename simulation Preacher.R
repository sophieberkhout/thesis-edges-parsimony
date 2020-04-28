# library("MASS")
library("dplyr")
library("bootnet")
library("psychonetrics")

# ------ Create network matrices ------ #
degree  <- getnw("degree")

close   <- getnw("close")
close2   <- getnw("close2")

between <- getnw("between")
between2 <- getnw("between2")

alt     <- getnw("alt")
alt2     <- getnw("alt2")

# ------------------------------------#


preacher <- function(models, niter = 1000, nobs = 100){
  
  dim <- nrow(models$full)
  
  LLs <- data.frame(full = numeric(),
                    a = numeric(),
                    b = numeric())
  
  for(i in 1:niter){
    covmat <- crossprod(matrix(runif(dim^2, -1 ,1), dim))
    
    mod_full  <- ggm(covs = covmat, nobs = nobs, covtype = "ML", omega = models$full) %>% runmodel
    mod_a     <- ggm(covs = covmat, nobs = nobs, covtype = "ML", omega = models$a) %>% runmodel
    mod_b     <- ggm(covs = covmat, nobs = nobs, covtype = "ML", omega = models$b) %>% runmodel
    
    LLs[i, "full"]  <- fit(mod_full)[1, 2]
    LLs[i, "a"]     <- fit(mod_a)[1, 2]
    LLs[i, "b"]     <- fit(mod_b)[1, 2]
    
  }
  
  return(LLs)
}

LL_degree  <- preacher(models = degree)
LL_close   <- preacher(models = close)
LL_between <- preacher(models = between)
LL_alt     <- preacher(models = alt)

LL_close2   <- preacher(models = close2)
LL_between2 <- preacher(models = between2)
LL_alt2     <- preacher(models = alt2)


# a is red, b is blue

plot(ecdf(LL_degree$full), main="ECDF Degree", xlab = "LL", ylab="")
lines(ecdf(LL_degree$a), col = "red")
lines(ecdf(LL_degree$b), col = "blue")
legend(-1100, .4, legend=c("Model A", "Model B",  "Full Model"), 
       col = c("red", "blue", "black"), lty = rep(1, 3), box.lty = 0)

plot(ecdf(LL_close$full), main="ECDF Closeness", xlab = "LL", ylab="")
lines(ecdf(LL_close$a), col = "red")
lines(ecdf(LL_close$b), col = "blue")
legend(-1100, .4, legend=c("Model A", "Model B",  "Full Model"), 
       col = c("red", "blue", "black"), lty = rep(1, 3), box.lty = 0)


# a should be more flexible than b
# red should be to right of blue
plot(ecdf(LL_between$full), main="ECDF Betweenness", xlab = "LL", ylab="")
lines(ecdf(LL_between$a), col = "red")
lines(ecdf(LL_between$b), col = "blue")
legend(-1100, .4, legend=c("Model A", "Model B",  "Full Model"), 
       col = c("red", "blue", "black"), lty = rep(1, 3), box.lty = 0)

plot(ecdf(LL_between2$full), main="ECDF Betweenness New", xlab = "LL", ylab="")
lines(ecdf(LL_between2$a), col = "red")
lines(ecdf(LL_between2$b), col = "blue")
legend(-1380, .4, legend=c("Model A", "Model B",  "Full Model"), 
       col = c("red", "blue", "black"), lty = rep(1, 3), box.lty = 0)

# a should be les flexible than b
# blue should be to right of red
plot(ecdf(LL_alt$full), main="ECDF Alternative Paths", xlab = "LL", ylab="")
lines(ecdf(LL_alt$a), col = "red")
lines(ecdf(LL_alt$b), col = "blue")
legend(-1380, .4, legend=c("Model A", "Model B",  "Full Model"), 
       col = c("red", "blue", "black"), lty = rep(1, 3), box.lty = 0)

plot(ecdf(LL_alt2$full))
lines(ecdf(LL_alt2$a), col = "red")
lines(ecdf(LL_alt2$b), col = "blue")


