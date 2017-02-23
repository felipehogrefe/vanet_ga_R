GenerateRGreedyIndividual <- function(M = matrix(), k, tTime){
  # returns a random individual
  set.seed(62)
  
  numberOfVehicles <- ncol(M)
  numberOfIntersections <- nrow(M)
  qtdIndividuals <- 0
  
  #initialize individual
  s <- c()
  
  #initialize tj
  tj <- c(rep.int(x = tTime, times = numberOfVehicles))
  
  while(qtdIndividuals < k){
    W <- c()
    #W <- sapply(1:numberOfIntersections, function(i) { sapply(1:numberOfVehicles, function(v){ vTime <- M[i,v] if(vTime > tj[v]){ vTime <- tj[v] }  wr = wr + vTime } append(W, wr))})
    
    for(i in 1:numberOfIntersections){
      wr <- 0
      for(v in 1:numberOfVehicles){
        vTime <- M[i,v]
        if(vTime > tj[v]){
          vTime <- tj[v]
        }
        wr = wr + vTime
      }
      W <- append(W, wr)
    }
    
    
    w <- match(sample(sort(W,decreasing = TRUE)[1:10], size=1),W)
    if(qtdIndividuals>0){
      while(w%in%s){
        w <- match(sample(sort(W,decreasing = TRUE)[1:10], size=1),W)
      }
    }
    
    s <- append(s, w)
    
    for(v in 1:numberOfVehicles){
      if(matrix[w,v]>=tj[v]){
        tj[v] <- 0
      }else{
        tj[v] <- tj[v] - matrix[w,v]
      }
      matrix[w,v] <- 0
    }
    qtdIndividuals = qtdIndividuals+1
  }
  
  s
}