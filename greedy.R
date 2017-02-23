GenerateRGreedyIndividual <- function(M = matrix(), k, tTime){
  # returns a random individual
  
  numberOfVehicles <- ncol(M)
  numberOfIntersections <- nrow(M)
  qtdIndividuals <- 0
  
  
  
  #initialize individual
  s <- c()
  
  #initialize tj
  tj <- c(rep.int(x = tTime, times = numberOfVehicles))
  
  while(qtdIndividuals < k){
    W <- c(rep.int(x = 0, times = numberOfIntersections))
  
    W <- sapply(1:numberOfIntersections, function(i){
      wr <- 0
      # v <- numberOfVehicles
      # wr <- calcWr(M,v,i,tj,wr)
      for(v in 1:numberOfVehicles){
        if(M[i,v] > tj[v]){
          wr = wr + tj[v]
        }else{
          wr = wr + M[i,v]
        }
      }
      wr
      
    })
    
    w <- match(sample(sort(W,decreasing = TRUE)[1:10], size=1),W)
    if(qtdIndividuals>0){
      while(w%in%s){
        w <- match(sample(sort(W,decreasing = TRUE)[1:10], size=1),W)
      }
    }
    
    s <- append(s, w)
    
   matrix[w,] <- sapply(1:numberOfVehicles, function(v){
      if(matrix[w,v]>=tj[v]){
        tj[v] <- 0
      }else{
        tj[v] <- tj[v] - matrix[w,v]
      }
      0
    })
    
    qtdIndividuals = qtdIndividuals+1
  }
  print(s)
  s
}

calcWr <- function(M,v,i,tj,wr){
  if(v>0){
    vTime <- M[i,v]
    if(vTime > tj[v]){
      vTime <- tj[v]
    }
    print(v)
    wr = calcWr(M,v-1,i,tj,wr) + vTime
    return(wr)
  }else{
    return(0)
  }
}