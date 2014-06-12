#' Analysis script for stand patterns
#' for analysis of Khao Yai Succession data
#' 
#' Scripts are intended to be run in order of file names
#'
#' @author Florian Hartig \url{http://florianhartig.wordpress.com/}

rm(list=ls(all=TRUE))
load(file = "data/cleanedData.RData")

########### spatial pattern ######################

# checks whether there is some difference in the spatial pattern of tree species in the different plots

library(spatstat)


plots <- unique(cleanedData$Plot)

png("results/paper/spatialPatterns-plots.png", width = 1000, height = 1000)

par(mfrow=c(4,4))

for (i in 1:length(plots)){
  
  selectedData <- cleanedData[cleanedData$Plot == plots[i] & !is.na(cleanedData$Dbh_a) & cleanedData$Dbh_a > 5,]
  attach(selectedData)
  pattern <- ppp(X , Y, window=owin(c(0,ceiling(max(X))),c(0,ceiling(max(Y)))))  
  
  fit <- kppm(pattern, ~1, "Thomas")
  
  plot(pattern, cex = 0.3, main = paste(Plot[1], Stage[2]))
  
  plot(envelope(fit, fun = "Gest"), main = paste(Plot[1], Stage[2]))
  
  detach(selectedData)
}

dev.off()

