#' Analysis script for community diversity
#' for analysis of Khao Yai Succession data
#' 
#' Scripts are intended to be run in order of file names
#'
#' @author Florian Hartig \url{http://florianhartig.wordpress.com/}

attach(cleanedData)

png("results/paper/CommunityStructure.png", width = 800, height = 450)
par(mfrow = c(1,2), cex = 1.5)
totalrichness = length(unique(as.numeric(Species)))
shannon <- foreach(i=1:3) %do% diversity(table(Species[as.numeric(Stage) == i]))
simpson <- foreach(i=1:3) %do% diversity(table(Species[as.numeric(Stage) == i]), index = "simpson")
richness <- foreach(i=1:3) %do% (length(unique(Species[as.numeric(Stage) == i])) / totalrichness)

tmp <- cbind(unlist(shannon), unlist(simpson), unlist(richness))

barplot(tmp, beside=T, names = c("Shannon", "Simpson", "Richness"), las = 2)
legend("topright", levels(Stage), fill = c("gray","darkgray", "black"))

# create community matrix per plot
communityMatrix <- table(Plot, Species)
# calculate Bray-Curtis distance among samples
comm.bc.dist <- vegdist(communityMatrix, method = "bray")
# cluster communities using average-linkage algorithm
comm.bc.clust <- hclust(comm.bc.dist, method = "average")
# plot cluster diagram
plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity", main = "Community similarity", xlab = "")
dev.off()




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


detach(cleanedData)


