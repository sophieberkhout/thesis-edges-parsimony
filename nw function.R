getnw <- function(type, half = FALSE){
  if(type == "degree"){
    nw <- matrix(0, 8, 8)
    nw[1, 2] <- nw[1, 5] <- 
      nw[2, 3] <- nw[2, 6] <- 
      nw[2, 7] <- nw[3, 4] <- 
      nw[3, 7] <- nw[4, 7] <- 
      nw[5, 6] <- nw[6, 7] <- 
      nw[7, 8] <- 1
  }
  
  if(type == "close"){
    nw <- matrix(0, 7, 7)
    nw[1, 2] <- nw[1, 5] <- 
      nw[2, 3] <- nw[2, 6] <- 
      nw[3, 4] <- nw[3, 7] <- 
      nw[4, 7] <- nw[5, 6] <- 
      nw[6, 7] <- 1
  }
  
  if(type == "between"){
    nw <- matrix(0, 7, 7)
    nw[1, 5] <- nw[2, 3] <- 
      nw[2, 6] <- nw[2, 7] <- 
      nw[3, 7] <- nw[4, 5] <- 
      nw[5, 6] <- nw[6, 7] <- 1
  }
  
  if(type == "alt"){
    nw <- matrix(0,6,6)
    nw[1, 2] <- nw[1, 4]  <- 
      nw[2, 3]<- nw[2, 5] <-
      nw[3, 5] <- nw[3, 6]  <- 
      nw[4, 5]  <- nw[5, 6]  <- 1
  }
  
  if(half == FALSE){
    nw[lower.tri(nw)] <- t(nw)[lower.tri(nw)]
  }
  
  return(nw)
}