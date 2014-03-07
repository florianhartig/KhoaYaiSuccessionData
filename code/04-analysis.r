#' Analysis script
#'
#' @author Florian Hartig \url{http://florianhartig.wordpress.com/}


attach(cleanedData)



png("results/paper/Allometries.png", width = 1000, height = 1000)
par(mfrow = c(2,2), cex = 1.3)

##############################################
# Analysis of diameter - height relationships



fitTH <- lmer(log10(TH) ~ log10(Dbh_a) * Stage + I(log10(Dbh_a)^2) + (1|Plot)) 
summary(fitTH)

cleanedData$predictedTreeHeight<- predict(fitTH, newdata = cleanedData)

plot(log10(Dbh_a), log10(TH), pch = as.numeric(Stage), xlim = c(0.5, 2), main = "Tree height")
points(log10(Dbh_a), cleanedData$predictedTreeHeight, col = Stage, cex = 0.2)
legend("bottomright", legend = c("Early", "Mid", "Late"), pch = 1:3, col = 1:3, lwd = 1)


################################################
#  Analysis Crown Area

fitCA <- lmer(log10(crownArea) ~ log10(Dbh_a) * Stage + I(log10(Dbh_a)^2) + (1|Plot)) 

summary(fitCA)

cleanedData$predictedCrownArea<- predict(fitCA, newdata = cleanedData)

plot(log10(Dbh_a), log10(crownArea), pch = as.numeric(Stage), xlim = c(0.5, 2), main = "Crown projection area")
points(log10(Dbh_a), cleanedData$predictedCrownArea, col = Stage, cex = 0.2)

legend("bottomright", legend = c("Early", "Mid", "Late"), pch = 1:3, col = 1:3, lwd = 1)


################################################
#  Analysis Crown Height

fitCH <- lmer(log10(crownHeight) ~ log10(Dbh_a) * Stage + I(log10(Dbh_a)^2) + (1|Plot)) 

summary(fitCH)

cleanedData$predictedCrownHeight<- predict(fitCH, newdata = cleanedData)

plot(log10(Dbh_a), log10(crownHeight), pch = as.numeric(Stage), xlim = c(0.5, 2), main = "Crown height")
points(log10(Dbh_a), cleanedData$predictedCrownHeight, col = Stage, cex = 0.2)


legend("bottomright", legend = c("Early", "Mid", "Late"), pch = 1:3, col = 1:3, lwd = 1)



################################################
#  Analysis Crown Volume

fitCV <- lmer(log10(crownVolume) ~ log10(Dbh_a) * Stage + I(log10(Dbh_a)^2) + (1|Plot)) 

summary(fitCV)

cleanedData$predictedCrownVolume<- predict(fitCV, newdata = cleanedData)

plot(log10(Dbh_a), log10(crownVolume), pch = as.numeric(Stage), xlim = c(0.5, 2), main = "Crown Volume")
points(log10(Dbh_a), cleanedData$predictedCrownVolume, col = Stage, cex = 0.2)
legend("bottomright", legend = c("Early", "Mid", "Late"), pch = 1:3, col = 1:3, lwd = 1)

dev.off()

detach(cleanedData)
attach(cleanedData)


##############################################
#Inclination

png("results/inclination.png", width = 1000, height = 350)

par(mfrow= c(1,3))

hist(inclination*360/2/pi)
    
plot(inclination~Stage, main = "Inclination")

plot(inclination[Dbh_a<6]~Stage[Dbh_a<6], main = "Inclination large trees")

dev.off()


fitI <- lmer(inclination ~ Stage + (1|Plot), data = cleanedData[cleanedData$Dbh_a<6,]) 
summary(fitI)

# nothing siginficant / worth mentioning!
# If at all, inclination is small




#########################################
# Analysis of DBH variability 

png("results/Variability.png", width = 1000, height = 350)
par(mfrow=c(1,3))
DBHDistributions <- foreach(i=1:3) %do% hist(Dbh_a[as.numeric(Stage) == i] , breaks = 0:100, ylim = c(0,1500))
dev.off()

png("results/paper/structuralVariability.png", width = 1000, height = 1000)
par(mfrow=c(3,2), cex = 1.3)

boxplot(log10(Dbh_a) ~ Stage, horizontal = T, notch = T, las=2, par(mar = c(3,7,3,3)), main = "Log10 measured DBH")
boxplot(predictedTreeHeight ~ Stage, horizontal = T, notch = T, las=2, par(mar = c(3,7,3,3)), main = "Log10 Predicted Tree Height")
boxplot(predictedCrownHeight ~ Stage, horizontal = T, notch = T, las=2, par(mar = c(3,7,3,3)), main = "Log10 Predicted Crown Height")
boxplot(log10(predictedCrownArea) ~ Stage, horizontal = T, notch = T, las=2, par(mar = c(3,7,3,3)), main = "Log10 Predicted Crown Area")
boxplot(log10(predictedCrownVolume) ~ Stage, horizontal = T, notch = T, las=2, par(mar = c(3,7,3,3)), main = "Log10 Predicted Crown Volume")
boxplot(inclination[Dbh_a<6]*360/2/pi ~ Stage[Dbh_a<6], horizontal = T, notch = T, las=2, par(mar = c(3,7,3,3)), main = "Measured Inclination")
dev.off()

# Ordination Analysis

temp = data.frame(cbind(Dbh_a, TH, crownHeight, crownArea, crownVolume))

prComp1 <- princomp(na.omit(temp[as.numeric(Stage) == 1,]))
prComp2 <- princomp(na.omit(temp[as.numeric(Stage) == 2,]))
prComp3 <- princomp(na.omit(temp[as.numeric(Stage) == 3,]))

png("results/paper/PCATreeStructure.png", width = 1500, height = 500)
par(mfrow=c(1,3))
biplot(prComp1, cex = c(0.2,1))
biplot(prComp2, cex = c(0.2,1))
biplot(prComp3, cex = c(0.2,1))
dev.off()

# Quantification of analysis of DBH spread for plots and successional stages

scaledIQR <- function(vec,f)return(IQR(vec[f], na.rm = T ) / median(vec[f], na.rm = T)) 

DBHSpreadPerStage <- foreach(i=1:3) %do% boot(Dbh_a[as.numeric(Stage) == i], scaledIQR, 1000)[1]
HeightSpreadPerStage <- foreach(i=1:3) %do% boot(cleanedData$predictedTreeHeight[as.numeric(Stage) == i], scaledIQR, 1000)
CrownAreaSpreadPerStage <- foreach(i=1:3) %do% boot(cleanedData$predictedCrownArea[as.numeric(Stage) == i], scaledIQR, 1000)
CrownVolumeSpreadPerStage <- foreach(i=1:3) %do% boot(cleanedData$predictedCrownVolume[as.numeric(Stage) == i], scaledIQR, 1000)
CrownHeightSpreadPerStage <- foreach(i=1:3) %do% boot(cleanedData$predictedCrownHeight[as.numeric(Stage) == i], scaledIQR, 1000)
InclinationSpreadPerStage <- foreach(i=1:3) %do% boot(inclination[as.numeric(Stage) == i], scaledIQR, 1000)


##########################
# Analysis of Diversity

png("results/paper/CommunityStructure.png", width = 1800, height = 1200)
par(mfrow = c(2,3), cex = 1.5)

totalrichness = length(unique(as.numeric(Species)))


shannon <- foreach(i=1:3) %do% diversity(table(Species[as.numeric(Stage) == i]))
simpson <- foreach(i=1:3) %do% diversity(table(Species[as.numeric(Stage) == i]), index = "simpson")
richness <- foreach(i=1:3) %do% (length(unique(Species[as.numeric(Stage) == i])) / totalrichness)

tmp <- cbind(unlist(shannon), unlist(simpson), unlist(richness))

barplot(tmp, beside=T, names = c("Shannon", "Simpson", "Richness"), las = 2)
legend("topright", levels(Stage))


# create community matrix per plot

communityMatrix <- table(Plot, Species)

# calculate Bray-Curtis distance among samples
comm.bc.dist <- vegdist(communityMatrix, method = "bray")
# cluster communities using average-linkage algorithm
comm.bc.clust <- hclust(comm.bc.dist, method = "average")

# plot cluster diagram

plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity", main = "Plots clustered according to Bray-Curtis dissimilarity")


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

boxplot(boot.out, horizontal=T, names = levels(Stage), las = 2, notch = T, par(mar = c(3,7,3,3)), main = "Bootstrapped difference in height between two \n random individuals and two random individuals of the same species")

par(mar = c(3,12,3,3))
out <- foreach(i=1:3) %do% barplot(sort(table(Species[as.numeric(Stage) == i]), decreasing = T, na.last = T)[1:10], las = 2, horiz = T, mar = c(3,8,3,3), cex.names = 0.9)


dev.off()

detach(cleanedData)

