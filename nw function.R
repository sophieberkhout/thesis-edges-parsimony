getnw <- function(type, half = FALSE){
# For all models, nw_b should fit better than nw_a.
# The first models are the models from the proposal.
# The models with a 2 are alternative models that should be similar.
  
  if(type == "degree"){
    nw <- matrix(0, 8, 8)
    nw[1, 2] <- nw[1, 5] <- 
      nw[2, 3] <- nw[2, 6] <- 
      nw[2, 7] <- nw[3, 4] <- 
      nw[3, 7] <- nw[4, 7] <- 
      nw[5, 6] <- nw[6, 7] <- 
      nw[7, 8] <- 1
    
    nw_a <- nw
    nw_a[2, 6] <- 0
    
    nw_b <- nw
    nw_b[3, 7] <- 0
  }
  
  if(type == "close"){
    nw <- matrix(0, 7, 7)
    nw[1, 2] <- nw[1, 5] <- 
      nw[2, 3] <- nw[2, 6] <- 
      nw[3, 4] <- nw[3, 7] <- 
      nw[4, 7] <- nw[5, 6] <- 
      nw[6, 7] <- 1
    
    nw_a <- nw
    nw_a[3, 7] <- 0
    
    nw_b <- nw
    nw_b[2, 6] <- 0
  }
  
  if(type == "between"){
    nw <- matrix(0, 7, 7)
    nw[1, 5] <- nw[2, 3] <- 
      nw[2, 6] <- nw[2, 7] <- 
      nw[3, 7] <- nw[4, 5] <- 
      nw[5, 6] <- nw[6, 7] <- 1
    
    nw_a <- nw
    nw_a[5, 6] <- 0
    
    nw_b <- nw
    nw_b[2, 7] <- 0
  }
  
  if(type == "alt"){
    nw <- matrix(0,6,6)
    nw[1, 2] <- nw[1, 4]  <- 
      nw[2, 3] <- nw[2, 5] <-
      nw[3, 5] <- nw[3, 6] <- 
      nw[4, 5] <- nw[5, 6] <- 1
    
    nw_a <- nw
    nw_a[1, 2] <- 0
    
    nw_b <- nw
    nw_b[3, 6] <- 0
  }
  
  if(type == "degree2"){
    nw <- matrix(0, 8, 8)
    nw[1, 2] <-  nw[2, 3] <-  
      nw[2, 4] <- nw[2, 5] <-   
      nw[3, 6] <- nw[5, 6] <-  
      nw[6, 7] <- nw[7, 8] <-  1
    
    nw_a <- nw
    nw_a[5, 6] <- 0
    
    nw_b <- nw
    nw_b[2, 5] <- 0
  }
  
  if(type == "close2"){
    nw <- matrix(0, 8, 8)
    nw[1, 2] <- nw[2, 3] <- 
      nw[2, 6] <- nw[3, 4] <- nw[3, 7] <-
      nw[4, 8] <- nw[5, 6] <-  
      nw[6, 7] <- nw[7, 8] <- 1
    
    nw_a <- nw
    nw_a[2, 6] <- 0
    
    nw_b <- nw
    nw_b[3, 7] <- 0
  }
  
  if(type == "between2"){
    nw <- matrix(0, 8, 8)
    nw[1, 5] <- nw[2, 3] <- 
      nw[2, 6] <- nw[2, 7] <- 
      nw[3, 7] <- nw[4, 8] <- 
      nw[5, 6] <- nw[6, 7] <- 
      nw[7, 8] <- 1
    
    nw_a <- nw
    nw_a[7, 8] <- 0
    
    nw_b <- nw
    nw_b[2, 6] <- 0
  }
  
  if(type == "alt2"){
    nw <- matrix(0,6,6)
    nw[1, 2] <- nw[1, 4] <- 
      nw[1, 5] <- nw[2, 3] <- 
      nw[2, 5] <- nw[2, 6] <- 
      nw[3, 5] <- nw[3, 6] <- 
      nw[5, 6] <- 1
    
    nw_a <- nw
    nw_a[2, 6] <- 0
    
    nw_b <- nw
    nw_b[2, 3] <- 0
  }

# For the Preacher method we need the full matrices.
# For the mimicry method we need half the matrices.
  
  if(half == FALSE){
    nw[lower.tri(nw)] <- t(nw)[lower.tri(nw)]
    nw_a[lower.tri(nw_a)] <- t(nw_a)[lower.tri(nw_a)]
    nw_b[lower.tri(nw_b)] <- t(nw_b)[lower.tri(nw_b)]
  }
  
# Put all the models in a list
  nwmodels <- list(full = nw,
                   a = nw_a,
                   b = nw_b)
  
  return(nwmodels)
}
