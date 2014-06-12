#' Analysis script for vertical stratification
#' for analysis of Khao Yai Succession data
#' 
#' Scripts are intended to be run in order of file names
#'
#' @author Florian Hartig \url{http://florianhartig.wordpress.com/}
#' 
#' 

rm(list=ls(all=TRUE))
load(file = "data/cleanedData.RData")

# NOTE : analysis is not finalized, I decided for the moment that it I won't run this
# to run this. The idea was to show that certain species appear more commonly in certain
# height classes, but because of the strong confoundment with plot I think we need to 
# account for plot run this analysis, not sure if this makes sense

get.average.distance.null.model <- function(continousVariable, Species, samples = 1000){
  
  MoreThanOneIndividual <- Species %in% names(which(table(Species) >1))
  
  speciesSelected <- sample(Species, samples, replace = T, prob = MoreThanOneIndividual)
  
  speciesValues <- matrix(nrow = samples, ncol = 2)
  randomValues <- matrix(nrow = samples, ncol = 2)
  
  for (i in 1:samples){
    
    speciesValues[i,] <- sample(continousVariable[Species == speciesSelected[i]],2)
    randomValues[i,] <- sample(continousVariable[MoreThanOneIndividual],2)
    
  } 
  distanceSpecies <- speciesValues[,1] - speciesValues[,2]
  distanceRandom <- randomValues[,1] - randomValues[,2]
  
  return(abs(distanceSpecies) - abs(distanceRandom))
}

get.average.distance.null.model <- cmpfun(get.average.distance.null.model)

out <- foreach(i=1:3) %do% (get.average.distance.null.model(10^predictedTreeHeight[as.numeric(Stage) == i], Species[as.numeric(Stage) == i], 3000)   )


boot.mean <- function(data, selection) return(mean(data[selection], na.rm =T))
boot.out <- foreach(i=1:3) %do% boot(out[[i]], boot.mean, R=1000)$t

