#' Analysis script for allometric relatinships
#' for analysis of Khao Yai Succession data
#' 
#' Scripts are intended to be run in order of file names
#'
#' @author Florian Hartig \url{http://florianhartig.wordpress.com/}
#' 

rm(list=ls(all=TRUE))
load(file = "data/cleanedData.RData")

attach(cleanedData)

png("results/paper/Allometries.png", width = 1000, height = 1000)
par(mfrow = c(2,2), cex = 1.3)

# Analysis of diameter - height relationships

fitTH <- lmer(log10(TH) ~ log10(Dbh_a) * Stage + I(log10(Dbh_a)^2) + (1|Plot)) 
summary(fitTH)
cleanedData$predictedTreeHeight<- predict(fitTH, newdata = cleanedData)

plot(log10(Dbh_a), log10(TH), pch = as.numeric(Stage), xlim = c(0.5, 2), main = "Tree height")
points(log10(Dbh_a), cleanedData$predictedTreeHeight, col = Stage, cex = 0.2)
legend("bottomright", legend = c("Early", "Mid", "Late"), pch = 1:3, col = 1:3, lwd = 1)

#  Analysis Crown Area

fitCA <- lmer(log10(crownArea) ~ log10(Dbh_a) * Stage + I(log10(Dbh_a)^2) + (1|Plot)) 
summary(fitCA)
cleanedData$predictedCrownArea<- predict(fitCA, newdata = cleanedData)

plot(log10(Dbh_a), log10(crownArea), pch = as.numeric(Stage), xlim = c(0.5, 2), main = "Crown projection area")
points(log10(Dbh_a), cleanedData$predictedCrownArea, col = Stage, cex = 0.2)
legend("bottomright", legend = c("Early", "Mid", "Late"), pch = 1:3, col = 1:3, lwd = 1)

#  Analysis Crown Height

fitCH <- lmer(log10(crownHeight) ~ log10(Dbh_a) * Stage + I(log10(Dbh_a)^2) + (1|Plot)) 
summary(fitCH)
cleanedData$predictedCrownHeight<- predict(fitCH, newdata = cleanedData)

plot(log10(Dbh_a), log10(crownHeight), pch = as.numeric(Stage), xlim = c(0.5, 2), main = "Crown height")
points(log10(Dbh_a), cleanedData$predictedCrownHeight, col = Stage, cex = 0.2)
legend("bottomright", legend = c("Early", "Mid", "Late"), pch = 1:3, col = 1:3, lwd = 1)

#  Analysis Crown Volume

fitCV <- lmer(log10(crownVolume) ~ log10(Dbh_a) * Stage + I(log10(Dbh_a)^2) + (1|Plot)) 
summary(fitCV)
cleanedData$predictedCrownVolume<- predict(fitCV, newdata = cleanedData)

plot(log10(Dbh_a), log10(crownVolume), pch = as.numeric(Stage), xlim = c(0.5, 2), main = "Crown Volume")
points(log10(Dbh_a), cleanedData$predictedCrownVolume, col = Stage, cex = 0.2)
legend("bottomright", legend = c("Early", "Mid", "Late"), pch = 1:3, col = 1:3, lwd = 1)

# cleaning up
dev.off()
detach(cleanedData)


results.allometrie <- data.frame(fixef(fitTH), sqrt(diag(vcov(fitTH))), 
                                 fixef(fitCA), sqrt(diag(vcov(fitCA))), 
                                 fixef(fitCH), sqrt(diag(vcov(fitCH))),
                                 fixef(fitCV), sqrt(diag(vcov(fitCV))) )

colnames(results.allometrie) = c("Dia-Hei", "SE.H" ,"Dia-CA", "SE.CA" ,
                                 "Dia-CH", "SE.CH" ,"Dia-CV", "SE.CV" )


save.image(file = "data/cleanedData2.RData")

