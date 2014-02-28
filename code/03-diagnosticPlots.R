#' Basic data exploration and diagnostics
#'
#' @author Florian Hartig \url{http://florianhartig.wordpress.com/}


draw.plots <- function(data){
  
  plotnames <- levels(data$Plot)
  number.plots <- length(plotnames)
  spacing <- ceiling(sqrt(number.plots))
  
  
  for (i in 1:number.plots){
    filename = paste("results/treePattern", plotnames[i], ".png", sep = "")
    png(filename, width = 1500, height = 1500)
    par(mfrow=c(2,2))
    temp = cleanedData[cleanedData$Plot == plotnames[i],]
    with(temp,{
      sel <- !is.na(TH)
      plot(X, Y, main = paste(Plot[1], Stage[1]), xlab = "X", ylab = "Y", cex = Dbh_a/10)
      points(X[sel], Y[sel], pch = 4, col = "red" , cex = TH[sel]/5)
      
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




