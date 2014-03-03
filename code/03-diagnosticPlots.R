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



attach(cleanedData)
  
  cleanedData$inclination <- asin(sqrt((Crown_X-Trunk_X)^2 + (Crown_Y - Trunk_Y)^2) / TH)
  
detach(cleanedData)




get.crown.points <- function(x){
  
  if (is.na(x$TH)){
    return(NA)
  } else {
    center <- c(x$Crown_X, x$Crown_Y)
    
    z1XY <- x$Crown_Z1 / sqrt(2)
    z2XY <- x$Crown_21 / sqrt(2)
    
    ratioY <- x$Width_Y / x$Crown_Y 
    ratioX <- x$Width_X / x$Crown_X
    
    # p1,3,5,7 are for vertical and horizontal lines
    # unclear with crownxy, check
    
    p1 <- c(0,x$Crown_Y)
    p2 <- center + c(-1,1) * z1XY 
    p3 <- c(x$Crown_X, x$Width_Y)
    p4 <- center + c(1,1) * z2XY 
    p5 <- c(x$Width_X,x$Crown_Y)
    p6 <- center + c(1,-1) * z1XY  
    p7 <- c(x$Crown_X,0)
    p8 <- center + c(-1,-1) * z2XY

    # removed the intermediate points for the moment because not clear how to split
    return(rbind(p1,p3,p4,p6,p8))
  }
}

crowncoordinates <- foreach(i=1:datarows) %do% get.crown.points(cleanedData[i,])

cleanedData$crownArea <- unlist(foreach(i=1:datarows) %do% if (is.na(crowncoordinates[[i]])) NA else areapl(crowncoordinates[[i]]))

# shapefactor assumes that F (flat), O (oval) and S (Spere) are esentially 

shapefactor <- c(2/3, 2/3, 1/3, 2/3, NA)


cleanedData$crownVolume <- cleanedData$crownArea*cleanedData$crownHeigh* 
  shapefactor[ifelse(is.na(cleanedData$Shape), 5,as.numeric(cleanedData$Shape))]
  
  
plot(cleanedData$Dbh_a, cleanedData$crownArea)
  