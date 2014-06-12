#' Analysis script for community diversity
#' for analysis of Khao Yai Succession data
#' 
#' Scripts are intended to be run in order of file names
#'
#' @author Florian Hartig \url{http://florianhartig.wordpress.com/}

rm(list=ls(all=TRUE))
load(file = "data/cleanedData.RData")

attach(cleanedData)


# --- community diversity indices ---
# might as well do this as a table

png("results/paper/DiversityIndices.png", width = 400, height = 400)
par(cex = 1.3)

totalrichness = length(unique(as.numeric(Species)))
shannon <- foreach(i=1:3) %do% diversity(table(Species[as.numeric(Stage) == i]))
simpson <- foreach(i=1:3) %do% diversity(table(Species[as.numeric(Stage) == i]), index = "simpson")
richness <- foreach(i=1:3) %do% (length(unique(Species[as.numeric(Stage) == i])) / totalrichness)

tmp <- cbind(unlist(shannon), unlist(simpson), unlist(richness))
barplot(tmp, beside=T, names = c("Shannon", "Simpson", "Richness"), las = 2, legend = levels(Stage), args.legend = list(x="topright"))
dev.off()

# --- community composition --- 
# Checks for similarity in the community matrix and then clusters the plots according to that

png("results/paper/CommunitySimilarity.png", width = 400, height = 400)
par(cex = 1.3)

# create community matrix per plot
communityMatrix <- table(Plot, Species)
# calculate Bray-Curtis distance among samples
comm.bc.dist <- vegdist(communityMatrix, method = "bray")
# cluster communities using average-linkage algorithm
comm.bc.clust <- hclust(comm.bc.dist, method = "average")
# plot cluster diagram

levels(cleanedData$Stage)
unique(cleanedData$Plot)

stagePlot = foreach(i=1:length(comm.bc.clust$labels))  %do% as.character(cleanedData$Stage[comm.bc.clust$labels[i] == cleanedData$Plot][1])

names = paste(comm.bc.clust$labels, " (", unlist(stagePlot), ")", sep = "")

plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity", main = "Community similarity", xlab = "", labels = names)

dev.off()

detach(cleanedData)


