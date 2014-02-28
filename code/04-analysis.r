#' Analysis script
#'
#' @author Florian Hartig \url{http://florianhartig.wordpress.com/}


attach(cleanedData)

#########################################
# Analysis of DBH variability 


png("results/DBHDistributions.png", width = 1000, height = 350)
par(mfrow=c(1,3))
DBHDistributions <- foreach(i=1:3) %do% hist(Dbh_a[as.numeric(Stage) == i] , breaks = 0:100, ylim = c(0,1500))
dev.off()

# Quantification of analysis of DBH spread for plots and successional stages

scaledIQR <- function(vec,f)return(IQR(vec[f], na.rm = T ) / median(vec[f], na.rm = T)) 

DBHSpreadPerStage <- foreach(i=1:3) %do% boot(Dbh_a[as.numeric(Stage) == i], scaledIQR, 1000)

DBHSpreadPerPlot <- foreach(i=1:8) %do% boot(Dbh_a[as.numeric(Plot) == i], scaledIQR, 1000)[1]


##############################################
# Analysis of diameter - height relationships


fit <- summary(lmer(log10(TH) ~ log10(Dbh_a) * Stage + I(log10(Dbh_a)^2) + (1|Plot))) 

plot(log10(Dbh_a), log10(TH))

summary(fit)










################################################
#  Analysis Crown allometry




calculate.crown.area <- function (crownFactors){
  
  
}



levels(Shape)

shapefactor <- list(F = 1, O = 1, P = 1, S = 1)

calculate.crown.volume <- function(baseArea, crownHeight, crownShape){
  return(baseArea*crownHeight*shapefactor[[as.numeric(crownShape)]])
}





##########################
# Analysis of Diversity


shannon <- foreach(i=1:3) %do% diversity(table(Species[as.numeric(Stage) == i]))
simpson <- foreach(i=1:3) %do% diversity(table(Species[as.numeric(Stage) == i]), index = "simpson")


detach(cleanedData)