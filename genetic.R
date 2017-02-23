Genetic <- function(M = matrix(),popSize,nRSUs,tTime){
  
  set.seed(62)
  
  greedyPopulationSize <- popSize/2
  randPopulationSize <- popSize - greedyPopulationSize
  numberOfVehicles <- ncol(M)
  numberOfIntersections <- nrow(M)
  
  population <- sapply(1:randPopulationSize, function(x) sample(1:numberOfIntersections,nRSUs))
  
  population <- cbind(population, sapply(1:greedyPopulationSize, function(x) GenerateRGreedyIndividual(M,nRSUs,tTime)))
  
  print(length(population))
}