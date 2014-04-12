#' Basic data exploration and diagnostics
#' for analysis of Khao Yai Succession data
#' 
#' Scripts are intended to be run in order of file names
#'
#' @author Florian Hartig \url{http://florianhartig.wordpress.com/}
#' @note can also be run during data cleaning to check for differences


draw.plots <- function(data){
  
  plotnames <- levels(data$Plot)
  number.plots <- length(plotnames)
  spacing <- ceiling(sqrt(number.plots))
  
  
  for (i in 1:number.plots){
    filename1 = paste("results/diagnostics/spatialPattern", plotnames[i], ".png", sep = "")
    filename2 = paste("results/diagnostics/treeData", plotnames[i], ".png", sep = "")
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
      
      if (sum(sel2) > 0){
        points(X[sel2], Y[sel2], pch = 6, col = "Purple" , cex = 4)
        text(X[sel2] + 3, Y[sel2], removedInconsistencies[sel2],  col = "Purple")
        text(X[sel2] + 3, Y[sel2] - 1.5, ID[sel2],  col = "Darkred", cex = 1.5)      
      }
      
      dev.off()
      
      png(filename2, width = 1000, height = 1500)
      
      par(mfrow=c(3,2), oma = c(15,10,10,10))
      
      plot(X, Y, main = "point pattern", xlab = "X", ylab = "Y", cex = Dbh_a/10, xlim = c(-2,82), ylim = c(-2,82))
      abline( h = c(0,60))
      abline( v = c(0,80))
      abline( h = c(20,40), lty = 2)
      abline( v = c(20,40,60), lty= 2)
      abline( h = seq(0,60,5), lty = 3, lwd = 0.5)
      abline( v = seq(0,80,5), lty = 3, lwd = 0.5)
      
      points(X[sel], Y[sel], pch = 2, col = "red" , cex = TH[sel]/5)
      points(X[sel], Y[sel], pch = 4, col = "green" , cex = inclination[sel] * 10)
      
      
      tmp <- hist(Dbh_a, breaks=100, plot=FALSE)
      tmp$count[tmp$count == 0] = 0.1
      bp <- barplot(tmp$count, log="y", col="white", ylim = c(0.1, 1000))

      plot(log10(Dbh_a), log10(TH), xlim = c(log10(1), log10(150)), ylim = c(log10(1), log10(35)))
      fit = lm( log10(TH)~log10(Dbh_a))
      abline(fit, col = "red")
      
      plot(log10(Dbh_a), log10(crownArea), xlim = c(log10(1), log10(150)), ylim = c(log10(1), log10(200)))
      fit = lm( log10(crownArea)~log10(Dbh_a))
      abline(fit, col = "red")      
      
      barplot(tail(sort(table(Species), decreasing = F, na.last = T),10), las = 2, horiz = T, mar = c(3,8,3,3), cex.names = 0.9)

      barplot(log10(sort(table(Species), decreasing = T, na.last = T) + 0.9))
      
   
      mtext(paste(Plot[1], Stage[1], "\n"), outer = T)
      
    })
    dev.off()
  }
}

draw.plots(cleanedData)



