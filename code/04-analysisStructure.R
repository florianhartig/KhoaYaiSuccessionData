#' Analysis script for community structure
#' for analysis of Khao Yai Succession data
#' 
#' Scripts are intended to be run in order of file names
#'
#' @author Florian Hartig \url{http://florianhartig.wordpress.com/}

attach(cleanedData)

png("results/paper/structuralVariability.png", width = 1000, height = 1000)
par(mfrow=c(2,1), cex = 1.3, oma = c(1,2,1,3))

allplot <- beanplot(log10(Dbh_a) ~ Stage,  what=c(TRUE,FALSE,FALSE,FALSE),show.names=FALSE , 
                    log="", main = "DBH / height structure", yaxt="n", ylim = c(0.4, 2.2) )
axis(2, at=log10(c(2,5,10,20,50,100)),labels=c(2,5,10,20,50,100), las = 2)
axis(2,at=log10(c(c(2:10), seq(20,100,10))),labels = F)
#mtext(2, ylab="DBH [cm]") 
axis(4, at=log10(c(2,5,10,20,50)),labels=c(2,5,10,20,50), las = T)
axis(4,at=log10(c(c(2:10), seq(20,50,10))),labels = F)
mtext("Height [m]", 4) 

beanplot(log10(Dbh_a) ~ Stage , log="", ll = 0.005, side = "first", col = "darkgrey", add = T) 
beanplot(predictedTreeHeight ~ Stage , log="", ll = 0.005, add = T, side = "second") 
legend("topleft", bty="n",c("Measured log10 DBH [cm]", "Predicted log10 tree height [m]"),
       fill = c("darkgrey", "black"))



allplot <- beanplot(predictedCrownHeight / predictedTreeHeight ~ Stage,  what=c(TRUE,FALSE,FALSE,FALSE),
                    show.names=FALSE , log="", main = "Crown area / height structure", yaxt="n", ylim = c(0,2.3))
axis(2, at=seq(0,1.1,0.2), las = T)
axis(4, at=log10(c(1,2,5,10,50,70,100, 200)),labels=c(1,2,5,10,20,5,100, 200), las = 2)
axis(4,at=log10(c(c(1:10), seq(20,100,10), 200)),labels = F)
beanplot(predictedCrownHeight / predictedTreeHeight ~ Stage , log="", ll = 0.005, side = "first", col = "darkgrey", add = T) 
beanplot(predictedCrownArea ~ Stage , log="", ll = 0.005, add = T, side = "second") 
legend("topleft", bty="n",c("Predicted relative crown height", "Predicted log10 crown area [m^2]"),
       fill = c("darkgrey", "black"))

dev.off()

detach(cleanedData)
