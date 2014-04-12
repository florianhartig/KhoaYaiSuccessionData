#' Analysis script for stand patterns
#' for analysis of Khao Yai Succession data
#' 
#' Scripts are intended to be run in order of file names
#'
#' @author Florian Hartig \url{http://florianhartig.wordpress.com/}

attach(cleanedData)

png("results/paper/standCharacteristics.png", width = 1000, height = 350)
par(mfrow=c(335,1))

density <- table(Stage, Plot)
barplot(density, las = 2, ylab = "Stem number", space = c(1,2))

stemcounts <- ddply(cleanedData, .(Stage, Plot), )
boxplot()

boot.median <- function(vec,f)return(median(vec[f], na.rm = T )) 
boot.comparison <- 

inclinationSpreadPerStage <- foreach(i=1:3) %do% boot(inclination[as.numeric(Stage) == i], boot.median, 1000)

boot.ci(inclinationSpreadPerStage[[1]], conf = c(0.90, 0.95))


lean <- foreach(i=1:3) %do% boot(inclination[as.numeric(Stage) == i], boot.median, 1000)


boot.basal.area <- function(dbhVector, f) return(sum(dbhVector[f]^2 / 4 * pi, na.rm = T))
basalAreaBoot <- foreach(i=1:8) %do% boot(Dbh_a[as.numeric(Plot) == i], boot.basal.area, 1000)

boot.ci(basalAreaBoot[[1]])
BAPlot foreach(i=1:8) %do% basalAreaBoot

boxplot(inclination*360/2/pi ~ Stage, horizontal = T, notch = T, las=2, par(mar = c(3,7,3,3)), main = "All")
boxplot(inclination[Dbh_a<6]*360/2/pi ~ Stage[Dbh_a<6], horizontal = T, notch = T, las=2, par(mar = c(3,7,3,3)), main = ">6cm")

fitI <- lmer(inclination ~ Stage + (1|Plot), data = cleanedData[cleanedData$Dbh_a<6,]) 
summary(fitI)

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

de99tach(cleanedData)