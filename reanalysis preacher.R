library("lavaan")
library("MASS")
library("corpcor")

# Create Model A
modelA <- '
V3 ~ V1 + V2
V1~~0*V2
'

# Create Model B
modelB <- '
V3 ~ V2
V2 ~ V1
V1~~V1
'

# firneasA and fitmeasB use only positive correlations
# firneasA.2 and fitmeasB.2 use both positive and negative correlations

# Create empty vectors to fill 
fitmeasA <- vector()
fitmeasB <- vector()

for(i in 1:1000){
  # Generate covariance matrix 
  covm <- crossprod(matrix(runif(3^2, 0.1, 1), 3))
  
  # Convert covariance matrix to correlation matrix
  corm<-cov2cor(covm)
  
  # Give matrix variable names
  colnames(corm) <- c("V1", "V2", "V3")
  rownames(corm) <- c("V1", "V2", "V3")
  
  # Generate data from correlation matrix
  dat <- mvrnorm(100, rep(0, 3), corm)
  
  test <- try({
    
    # Fit Model A and Model B
    fitA <- sem(model = modelA, data = dat)
    fitB <- sem(model = modelB, data = dat)
    
    # Extract the fit measures 
    fitmeasA[i] <- fitMeasures(fitA, fit.measures = "logl")
    fitmeasB[i] <- fitMeasures(fitB, fit.measures = "logl")

  })
  
  if(class(test) == "try-error") next

}

# Get the means and sds with only positive correlations
mean(fitmeasA, na.rm = T)
sd(fitmeasA, na.rm = T)

mean(fitmeasB, na.rm = T)
sd(fitmeasB, na.rm = T)


# Get means and sds for both negative and positive correlations
mean(fitmeasA.2, na.rm = T)
sd(fitmeasA.2, na.rm = T)

mean(fitmeasB.2, na.rm = T)
sd(fitmeasB.2, na.rm = T)

# Format data to plot
df <- data.frame(a = fitmeasA2$logl, b = fitmeasB$logl)
df <- na.omit(df)
df2 <- tidyr::gather(df, Model, Fit)
myplot(df2)

# Format data.2 to plot
df.2 <- data.frame(a = fitmeasA2.2$logl, b = fitmeasB.2$logl)
df.2 <- na.omit(df.2)
df2.2 <- tidyr::gather(df.2, Model, Fit)
myplot(df2.2)

