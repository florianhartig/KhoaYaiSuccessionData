#' Basic data exploration and diagnostics
#'
#' @author Florian Hartig \url{http://florianhartig.wordpress.com/}
#' @note can / should also be run during data cleaning


draw.plots <- function(data){
  
  plotnames <- levels(data$Plot)
  number.plots <- length(plotnames)
  spacing <- ceiling(sqrt(number.plots))
  
  
  for (i in 1:number.plots){
    filename1 = paste("results/spatialPattern", plotnames[i], ".png", sep = "")
    filename2 = paste("results/treeData", plotnames[i], ".png", sep = "")
    png(filename1, width = 1500, height = 1500)
    temp = cleanedData[cleanedData$Plot == plotnames[i],]
    with(temp,{
      sel <- !is.na(TH)
      plot(X, Y, main = paste(Plot[1], Stage[1]), xlab = "X", ylab = "Y", cex = Dbh_a/10, xlim = c(-2,82), ylim = c(-2,82))
      abline( h = c(0,60))
      abline( v = c(0,80))
      abline( h = c(20,40), lty = 2)
      abline( v = c(20,40,60), lty= 2)
      abline( h = seq(0,60,5), lty = 3, lwd = 0.5)
      abline( v = seq(0,80,5), lty = 3, lwd = 0.5)
      
      points(X[sel], Y[sel], pch = 2, col = "red" , cex = TH[sel]/5)
      
      points(X[sel], Y[sel], pch = 4, col = "green" , cex = inclination[sel] * 10)
      
      sel2 <- removedInconsistencies != ""
      
      points(X[sel2], Y[sel2], pch = 6, col = "Purple" , cex = 4)
      text(X[sel2] + 3, Y[sel2], removedInconsistencies[sel2],  col = "Purple")
      text(X[sel2] + 3, Y[sel2] - 1.5, ID[sel2],  col = "Darkred", cex = 1.5)
      
      
      
      dev.off()
      
      png(filename2, width = 1500, height = 1500)
      par(mfrow=c(2,2))
      
      hist(Dbh_a, breaks = 100)
      hist(TH, breaks = 100)
      plot(log10(Dbh_a), log10(TH), xlim = c(log10(1), log10(150)), ylim = c(log10(1), log10(35)))
      fit = lm( log10(TH)~log10(Dbh_a))
      abline(fit, col = "red")
      
    })
    dev.off()
  }
}

draw.plots(cleanedData)






plot(cleanedData$Dbh_a, cleanedData$H_1stB, col = cleanedData$Plot)

plot(cleanedData$Dbh_a, cleanedData$Crown_Z1, col = cleanedData$Plot)
legend("topright", legend = as.character(levels(cleanedData$Plot)), pch = 1, col = palette())

plot(cleanedData$Dbh_a, cleanedData$Crown_Z2, col = cleanedData$Plot)
legend("topright", legend = as.character(levels(cleanedData$Plot)), pch = 1, col = palette())

plot(cleanedData$Dbh_a, cleanedData$Width_X, col = cleanedData$Plot)
legend("topright", legend = as.character(levels(cleanedData$Plot)), pch = 1, col = palette())


plot(cleanedData$Dbh_a, cleanedData$Width_Y, col = cleanedData$Plot)
legend("topright", legend = as.character(levels(cleanedData$Plot)), pch = 1, col = palette())

plot(cleanedData$Dbh_a, cleanedData$Crown_X, col = cleanedData$Plot)
legend("topright", legend = as.character(levels(cleanedData$Plot)), pch = 1, col = palette())

plot(cleanedData$Dbh_a, cleanedData$Crown_Y, col = cleanedData$Plot)
legend("topright", legend = as.character(levels(cleanedData$Plot)), pch = 1, col = palette())

plot(cleanedData$Dbh_a, cleanedData$crownArea, col = cleanedData$Plot)


plot(cleanedData$Dbh_a, cleanedData$crownHeight, col = cleanedData$Plot)
plot(cleanedData$Dbh_a, cleanedData$relativeCrownHeight, col = cleanedData$Plot)
boxplot(cleanedData$relativeCrownHeight ~ cleanedData$Shape + cleanedData$Plot, las = 2, col = rep(c(1,2,3,4,5),8))


head(cleanedData[cleanedData$removedInconsistencies!="",])


