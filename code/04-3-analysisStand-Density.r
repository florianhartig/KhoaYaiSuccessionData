#' Analysis script for stand patterns
#' for analysis of Khao Yai Succession data
#' 
#' Scripts are intended to be run in order of file names
#'
#' @author Florian Hartig \url{http://florianhartig.wordpress.com/}
#' 

library(gplots)
library(car)

rm(list=ls(all=TRUE))
load(file = "data/cleanedData.RData")

attach(cleanedData)

png("results/paper/standDensityLean.png", width = 800, height = 500)

par(mfrow=c(1,2))


  # STEM DENSITY #

  density <- table(Stage, Plot)
  barplot(density, las = 1, xlab = "Stem number", space = 1, legend = c("Establishment", "Exclusion", "Old-growth"), horiz = T, main = "Stem number")


  # LEAN #
  
  boot.median <- function(vec,f)return(median(vec[f], na.rm = T )) 
  leanPerPlot <- foreach(i=1:8) %do% boot(inclination[as.numeric(Plot) == i & Dbh_a>7], boot.median, 10000)
  
  bestEstimateLean <- unlist(foreach(i=1:8) %do% leanPerPlot[[i]]$t0)
  
  confintervals <- foreach(i=1:8) %do% boot.ci(leanPerPlot[[i]], type = "bca")
  
  CI <- matrix(unlist(foreach(i=1:8) %do% confintervals[[i]]$bca[1,4:5]), nrow = 2, byrow = F)
  barplot2(bestEstimateLean, ci.l = CI[1,] ,ci.u = CI[2,], plot.ci = T, horiz = T, space = 1, main = "Lean [dbh > 7]", xlab = "Inclination [degrees]")
  

dev.off()


# differences NS
fitI <- lmer(inclination ~ Stage + (1|Plot), data = cleanedData[cleanedData$Dbh_a>6,]) 
Anova(fitI)

detach(cleanedData)
