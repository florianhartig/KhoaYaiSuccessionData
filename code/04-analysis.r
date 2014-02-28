#' Analysis script
#'
#' @author Florian Hartig \url{http://florianhartig.wordpress.com/}


attach(cleanedData)

#########################################
# variability in DBH

phases = levels(Stage)

par(mfrow=c(1,3))

# absolut
hist(Dbh_a[Stage == phases[1]] / median(Dbh_a[Stage == phases[1]]), breaks = seq(0,5,0.1), ylim = c(0,1500))
hist(Dbh_a[Stage == phases[2]] / median(Dbh_a[Stage == phases[1]]), breaks = seq(0,5,0.1), ylim = c(0,1500))
hist(Dbh_a[Stage == phases[3]] / median(Dbh_a[Stage == phases[1]]), breaks = seq(0,5,0.1), ylim = c(0,1500))

# relative 
hist((Dbh_a[Stage == phases[1]] / median(Dbh_a[Stage == phases[1]], na.rm = T)), )
hist((Dbh_a[Stage == phases[2]] / median(Dbh_a[Stage == phases[2]], na.rm = T)))
hist((Dbh_a[Stage == phases[3]] / median(Dbh_a[Stage == phases[3]], na.rm = T)))



scaledIQR <- function(vec,f)return(IQR(vec[f], na.rm = T ) / median(vec[f], na.rm = T)) 
  
boot(Dbh_a[Stage == phases[1]], scaledIQR, 1000)[1]  
boot(Dbh_a[Stage == phases[2]], scaledIQR, 1000)
boot(Dbh_a[Stage == phases[3]], scaledIQR, 1000)


SpreadPlots <- foreach(i=1:8) %do% boot(Dbh_a[as.numeric(Plot) == i], scaledIQR, 1000)[1]

levels(Plot)


fit <- summary(lmer(log10(TH) ~ log10(Dbh_a) * Stage + I(log10(Dbh_a)^2) + (1|Plot))) 

plot(log10(Dbh_a), log10(TH))

summary(fit)





detach(cleanedData)




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
# Diversity


shannon <- foreach(i=1:3) %do% diversity(table(Species[as.numeric(Stage) == i]))
simpson <- foreach(i=1:3) %do% diversity(table(Species[as.numeric(Stage) == i]), index = "simpson")

boot







# Folder for saving files

FS = "D:/Researches/Tree Architecture/Data_Oct11/Data_Analysis/Analysis/Res_Fig/"
FigS_W = 900 # width of scatter plot figure
FigS_H = 700 # height of scatter plot figure
FigS_R = 150 # resolution of scatter plot figure

# Calculate crown area
  Dat$CrArea <- (1/4)*pi*Dat[,6]*Dat[,7]

# Find the longest length of crow axis, generally X is longer than Y (but not always)
CrL = ifelse(Dat[,6] < Dat[,7],Dat[,7],Dat[,6])  
  
tiff(file=paste(FS,"CH.tif",sep=""), width =400, height =500)
# Crown Height
boxplot(Dat[,4] ~Dat[,2],xlab="Stage",ylab="Crown Height (m)", 
              main="Crown height",names=c("St1","St2","St3"))
dev.off()

tiff(file=paste(FS,"TH.tif",sep=""), width =400, height =500)
# Tree Height
boxplot(Dat[,5] ~Dat[,2],xlab="Stage",ylab="Tree height (m)", 
              main="Tree height",names=c("St1","St2","St3"))
dev.off()

tiff(file=paste(FS,"CrPerTH.tif",sep=""), width =400, height =500)
# Crown Height/Tree Height
 boxplot(Dat[,4]/Dat[,5] ~Dat[,2],xlab="Stage",ylab="Crown/Tree height (m)",
               main="Crown height/Tree height",names=c("St1","St2","St3"))
 dev.off()
 
 
# Ratio beetween Crown the longest crown
# Use only the longest, generally X is longer than Y
CrL = ifelse(Dat[,6] < Dat[,7],Dat[,7],Dat[,6])
tiff(file=paste(FS,"CrL.tif",sep=""), width =400, height =500)
# Longest crown length
boxplot(CrL ~Dat[,2],xlab="Stage",ylab="Longest crown width(m)", main="Longest width of Crown axis")
dev.off()


tiff(file=paste(FS,"CrLPerCH.tif",sep=""), width =400, height = 500)
# Longest crownlength vs crown heigth ratio
 boxplot(CrL/Dat[,4] ~Dat[,2],xlab="Stage",ylab="Crown width ratio(m)", 
          main="Longest Crown axis/Crown Heigth",names=c("St1","St2","St3"))
dev.off()


tiff(file=paste(FS,"CrLPerTH.tif",sep=""), width =400, height = 500)
## Longest crownlength vs tree heigth ratio
 boxplot(CrL/Dat[,5] ~Dat[,2],xlab="Stage",ylab="Crown width ratio(m)", 
          main="Longest Crown axis/Tree Heigth",names=c("St1","St2","St3"))
 dev.off()


tiff(file=paste(FS,"CrArea.tif",sep=""), width =450, height = 500) 
#crown shading (projection) area 
  boxplot(Dat$CrArea ~Dat[,2],xlab="Stage",ylab="Crown area(sq.m)", 
              main="Crown shading area",names=c("St1","St2","St3"))
dev.off()

tiff(file=paste(FS,"CrAreaPerCH.tif",sep=""), width =450, height = 500) 
# crown projection stadardized by crown height
 boxplot (Dat$CrArea/Dat[,4] ~Dat[,2],xlab="Stage",ylab="Standardized crown area", 
              main="Crown shading area Standardized by crown height",names=c("St1","St2","St3")) 
dev.off()
              
tiff(file=paste(FS,"CrAreaPerTH.tif",sep=""), width =450, height = 500)
# crown projection stadardized by tree height
 boxplot(Dat$CrArea/Dat[,5] ~Dat[,2],xlab="Stage",ylab="Standardized crown area", 
              main="Crown shading area standardized by tree height",names=c("St1","St2","St3")) 
dev.off()                           
 
 ######## Scatter plot ##################
 
 # Tree Height VS crown Height
tiff(file=paste(FS,"Scatt_TH_CH.tif",sep=""), width =FigS_W, height = FigS_H, res=FigS_R)  
plot(Dat[which(Dat[,2]==3),5],Dat[which(Dat[,2]==3),4],xlab="Tree Height",ylab="Crown height (m)", main="Tree height vs crown height", col = "red")
 points(Dat[which(Dat[,2]==2),5],Dat[which(Dat[,2]==2),4], col = "blue")
 points(Dat[which(Dat[,2]==1),5],Dat[which(Dat[,2]==1),4], col = "green") 
 legend("topleft",legend=c("St1","St2","St3"),col=c(3,4,2),pch=c(1,1,1))
dev.off()
 
 # ratio Crown Height/Tree Height VS Tree Heigth
 tiff(file=paste(FS,"Scatt_TH_CHPerTH.tif",sep=""), width =FigS_W, height = FigS_H, res=FigS_R)  
  plot(Dat[which(Dat[,2]==3),5],(Dat[which(Dat[,2]==3),4]/Dat[which(Dat[,2]==3),5]),xlab="Tree Height",ylab="Ratio Crown height (m)",
         main="Tree height vs Ratio crown height", col = "red")
 points(Dat[which(Dat[,2]==2),5],Dat[which(Dat[,2]==2),4]/Dat[which(Dat[,2]==2),5], col = "blue")
 points(Dat[which(Dat[,2]==1),5],Dat[which(Dat[,2]==1),4]/Dat[which(Dat[,2]==1),5], col = "green") 
 legend("topright",legend=c("St1","St2","St3"),col=c(3,4,2),pch=c(1,1,1))
 dev.off()
 
 # Ratio beetween tree hegiht and the longest crown width
# Use only the longest, generally X is longer than Y          
# Stage1
CrL1 = ifelse(Dat[which(Dat[,2]==1),6] < Dat[which(Dat[,2]==1),7],Dat[which(Dat[,2]==1),7],Dat[which(Dat[,2]==1),6])
# Stage2
CrL2 = ifelse(Dat[which(Dat[,2]==2),6] < Dat[which(Dat[,2]==2),7],Dat[which(Dat[,2]==2),7],Dat[which(Dat[,2]==2),6])
# Stage3
CrL3 = ifelse(Dat[which(Dat[,2]==3),6] < Dat[which(Dat[,2]==3),7],Dat[which(Dat[,2]==3),7],Dat[which(Dat[,2]==3),6])

tiff(file=paste(FS,"Scatt_TH_CrL.tif",sep=""), width =FigS_W, height = FigS_H, res=FigS_R) 
plot(Dat[which(Dat[,2]==3),5],CrL3,xlab="Tree Height (m)",ylab="The longest width of crown",
         main="Tree height vs the longest crown (m)", col = "red")
 points(Dat[which(Dat[,2]==2),5],CrL2, col = "blue")
 points(Dat[which(Dat[,2]==1),5],CrL1, col = "green") 
 legend("topleft",legend=c("St1","St2","St3"),col=c(3,4,2),pch=c(1,1,1))
dev.off() 
 
 #Crown height and the longest crown width
 tiff(file=paste(FS,"Scatt_CH_CrL.tif",sep=""), width =FigS_W, height = FigS_H, res=FigS_R) 
 plot(Dat[which(Dat[,2]==3),4],CrL3 ,xlab="Crown height (m)",ylab="The longest width of crown",
         main="Crown height vs the longest crown (m)", col = "red")
 points(Dat[which(Dat[,2]==2),4],CrL2, col = "blue")
 points(Dat[which(Dat[,2]==1),4],CrL1, col = "green") 
 legend("topleft",legend=c("St1","St2","St3"),col=c(3,4,2),pch=c(1,1,1))
 dev.off()
 #Standardize by tree height 
 
 tiff(file=paste(FS,"Scatt_CH_TH.tif",sep=""), width =FigS_W, height = FigS_H, res=FigS_R)
 plot(Dat[which(Dat[,2]==3),4]/Dat[which(Dat[,2]==3),5],CrL3 ,xlab="Standardized crown height (m)",ylab="The longest width of crown",
         main="Standardized crown height vs the longest crown (m)", col = "red")
 points(Dat[which(Dat[,2]==2),4]/Dat[which(Dat[,2]==2),5],CrL2, col = "blue")
 points(Dat[which(Dat[,2]==1),4]/Dat[which(Dat[,2]==1),5],CrL1, col = "green") 
 legend("topleft",legend=c("St1","St2","St3"),col=c(3,4,2),pch=c(1,1,1))
dev.off() 
  # Tree Height VS Crown Shading area

tiff(file=paste(FS,"Scatt_TH_CrArea.tif",sep=""), width =FigS_W, height = FigS_H, res=FigS_R)  
plot(Dat[which(Dat[,2]==3),5],Dat[which(Dat[,2]==3),9],xlab="Tree Height",ylab="Crown area (sq. m)",
 main="Tree height vs crown projection area", col = "red")
 points(Dat[which(Dat[,2]==2),5],Dat[which(Dat[,2]==2),9], col = "blue")
 points(Dat[which(Dat[,2]==1),5],Dat[which(Dat[,2]==1),9], col = "green") 
 legend("topleft",legend=c("St1","St2","St3"),col=c(3,4,2),pch=c(1,1,1))
dev.off() 
 # Crown height VS Crown Shading area

tiff(file=paste(FS,"Scatt_CH_CrArea.tif",sep=""), width =FigS_W, height = FigS_H, res=FigS_R)    
plot(Dat[which(Dat[,2]==3),4],Dat[which(Dat[,2]==3),9],xlab="Crown height (m) ",ylab="Crown area (sq. m)",
 main="Crown height vs crown projection area", col = "red")
 points(Dat[which(Dat[,2]==2),4],Dat[which(Dat[,2]==2),9], col = "blue")
 points(Dat[which(Dat[,2]==1),4],Dat[which(Dat[,2]==1),9], col = "green") 
 legend("topleft",legend=c("St1","St2","St3"),col=c(3,4,2),pch=c(1,1,1))
dev.off() 
 
 
 