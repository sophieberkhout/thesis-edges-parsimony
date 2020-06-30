library("MASS")
library("dplyr")
library("bootnet")
library("psychonetrics")
library("ggplot2")
source("myplot.R")

# Create adjacancy matrix exploratory model 1
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

# Create adjacancy matrix exploratory model 2
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


formatsym <- function(nw, nw_a, nw_b, half = F){
  
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

# Make the ajacancy matrices symmetrical and format matrices
nw1 <- formatsym(exp1, exp1_a, exp1_b)
nw2 <- formatsym(exp2, exp2_a, exp2_b)

# Format matrices for mimicry analysis
nw1_half <- formatsym(exp1, exp1_a, exp1_b, half = T)
nw2_half <- formatsym(exp2, exp2_a, exp2_b, half = T)

# ----- method 1 ------ #

LL_nw1 <- preacher(nw1)
LL_nw2 <- preacher(nw2)

myplot(LL_nw1)

colMeans(subset(LL_nw1, Model == "A", Fit))
apply(subset(LL_nw1, Model == "A", Fit), 2, sd)
colMeans(subset(LL_nw1, Model == "B", Fit))
apply(subset(LL_nw1, Model == "B", Fit), 2, sd)
t.test(subset(LL_nw1, Model == "A", Fit)$Fit, 
       subset(LL_nw1, Model == "B", Fit)$Fit, 
       paired = T, alternative = "two.sided")

myplot(LL_nw2)

colMeans(subset(LL_nw2, Model == "A", Fit))
apply(subset(LL_nw2, Model == "A", Fit), 2, sd)
colMeans(subset(LL_nw2, Model == "B", Fit))
apply(subset(LL_nw2, Model == "B", Fit), 2, sd)
t.test(subset(LL_nw2, Model == "A", Fit)$Fit, 
       subset(LL_nw2, Model == "B", Fit)$Fit, 
       paired = T, alternative = "two.sided")

# --------------------- #

# --------- method 2 ----------- #
test_nw1 <- mimicry(nw1_half, nedges = 14-1, test = TRUE)
test_nw2 <- mimicry(nw2_half, nedges = 13-1, test = TRUE)


# means and sds model 1
difnw1 <- getdif(test_nw1$fitsep)
# difnw1 <- difnw1[which(difnw1$Fit < 2000),]
colMeans(subset(difnw1, Model == "Ab-Bb", Fit), na.rm = T)
colMeans(subset(difnw1, Model == "Ba-Aa", Fit), na.rm = T)

apply(subset(difnw1, Model == "Ab-Bb", Fit), 2, sd, na.rm = T)
apply(subset(difnw1, Model == "Ba-Aa", Fit), 2, sd, na.rm = T)


# means and sds model 2
difnw2 <- getdif(test_nw2$fitsep)
# difnw2 <- difnw2[difnw2$Fit < 2000 & difnw2$Fit > -2000,]
colMeans(subset(difnw2, Model == "Ab-Bb", Fit), na.rm = T)
colMeans(subset(difnw2, Model == "Ba-Aa", Fit), na.rm = T)

apply(subset(difnw2, Model == "Ab-Bb", Fit), 2, sd, na.rm = T)
apply(subset(difnw2, Model == "Ba-Aa", Fit), 2, sd, na.rm = T)



testedge <- function(dat){
  testedgevals <-  tidyr::gather(dat$fitsep, Model, Fit)
  edgename <- colnames(dat$edge_values)[ncol(dat$edge_values)]
  dat$edge_values <- dat$edge_values %>%
    rename(z = edgename)
  testedgevals$edgevals <- rep(dat$edge_values$z, 4)
  
  
  ggplot(testedgevals, aes(x = edgevals, y = Fit, colour = Model)) + facet_wrap(~Model) +
    geom_point(colour = "grey") + 
    geom_smooth(method = "lm", se = F) +
    theme_classic() + 
    xlab("Edge Parameter") +
    ylab("LL") +
    theme(legend.position = "none")
}

testedge(test_nw1)
testedge(test_nw2)
