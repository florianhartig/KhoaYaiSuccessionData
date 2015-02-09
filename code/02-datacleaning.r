#' Data preparation and cleaning
#' for analysis of Khao Yai Succession data
#' 
#' Scripts are intended to be run in order of file names
#'
#' @author Florian Hartig \url{http://florianhartig.wordpress.com/}
#' 
#' paths are relative to code directory
#' 
#' 

rm(list=ls(all=TRUE))

source("01-packages.r")

# reading Plot data
baseData <-read.csv("../data/CrHi_Map4.csv")
speciesData <- read.csv("../data/Species_List.csv")

# create output folders
dir.create("../results/paper", showWarnings = FALSE)
dir.create("../results/diagnostics", showWarnings = FALSE)



# Reordering, Calculations, Matching in new df

cleanedData = baseData
datarows = nrow(cleanedData)
heightMeasured = !is.na(cleanedData$TH)

# mapping species names to ID
getName <- function(index){
  return(paste(speciesData[index,1], speciesData[index,2]))
}

cleanedData$Species = as.factor(sapply(as.character(baseData$Species_ID), getName))

# removing unneccesary columns and renaming 
cleanedData$Plot_ID = NULL # redundant with Abbrev
cleanedData$Species_ID = NULL
colnames(cleanedData)[which(colnames(cleanedData) == "Abbrev")] = "Plot"
colnames(cleanedData)[which(colnames(cleanedData) == "ID_Tree_Table")] = "ID"
cleanedData$Stage = as.factor(cleanedData$Stage)
levels(cleanedData$Stage) = c("Establishment", "Exclusion", "Old-Growth")
cleanedData$Shape[cleanedData$Shape == ""] = NA
cleanedData$Shape = factor(cleanedData$Shape)
cleanedData$removedInconsistencies= rep("", datarows) # for consistency checks


# renaming plot names and States

# Before [1] "DumpSF" "KK1SF"  "KK2SF"  "KKOF"   "KSOF"   "KSSF"   "Res1"   "Res2" 

# Res1=SIS1; Res2=SIS2;
# DumpSF=SIS3; KK1SF=SES1; KK2SF=SES2; KSSF=SES3; KKOF=OGS1
# KSOF=OGS2

levels(cleanedData$Plot) = c("SIS3", "SES1", "SES2", "OGS1", "OGS2","SES3", "SIS1", "SIS2")

cleanedData$Plot = factor(cleanedData$Plot,levels(cleanedData$Plot)[c(7,8,1,2,3,6,4,5)])

levels(cleanedData$Stage) = c("Stand initiation", "Stem exclusion", "Old-growth")

######################
# HANDLE TREE XY COORDINATES

# check for both x an x_minus, if so, keep one (random) of them

indices <- which(!is.na(cleanedData$X+cleanedData$X_minus))
cleanedData$removedInconsistencies[indices] <- paste(cleanedData$removedInconsistencies[indices], "X and Xminus", cleanedData$X[indices], cleanedData$X_minus[indices] )
for (i in indices) if (sample.int(2,1) == 1) cleanedData$X[i] = NA else cleanedData$X_minus[i] = NA

indices <- which(!is.na(cleanedData$Y+cleanedData$Y_minus))
cleanedData$removedInconsistencies[indices] <- paste(cleanedData$removedInconsistencies[indices], "Y and Yminus", cleanedData$Y[indices], cleanedData$Y_minus[indices] )
for (i in indices) if (sample.int(2,1) == 1) cleanedData$Y[i] = NA else cleanedData$Y_minus[i] = NA

# check for too large coordinates, if so, assign random position within subquadrat

resampleColumn <- function(vectorX) return(sample(vectorX, datarows, replace = T))

indices <- which(cleanedData$X > 5)
cleanedData$removedInconsistencies[indices] <- paste(cleanedData$removedInconsistencies[indices], "X>5", cleanedData$X[indices])
cleanedData$X[indices] <- sample(cleanedData$X[cleanedData$X < 5], length(indices), replace = T)

indices <- which(cleanedData$X_minus > 5)
cleanedData$removedInconsistencies[indices] <- paste(cleanedData$removedInconsistencies[indices], "X_minus>5", cleanedData$X_minus[indices])
cleanedData$X_minus[indices] <- sample(cleanedData$X_minus[cleanedData$X_minus < 5], length(indices), replace = T)

indices <- which(cleanedData$Y > 5)
cleanedData$removedInconsistencies[indices] <- paste(cleanedData$removedInconsistencies[indices], "X>5", cleanedData$Y[indices])
cleanedData$Y[indices] <- sample(cleanedData$Y[cleanedData$Y < 5], length(indices), replace = T)

indices <- which(cleanedData$Y_minus > 5)
cleanedData$removedInconsistencies[indices] <- paste(cleanedData$removedInconsistencies[indices], "X_minus>5", cleanedData$Y_minus[indices])
cleanedData$Y_minus[indices] <- sample(cleanedData$Y_minus[cleanedData$Y_minus < 5], length(indices), replace = T)

# change to absolute coordinates

cleanedData$X[is.na(cleanedData$X)] = 0
cleanedData$Y[is.na(cleanedData$Y)] = 0
cleanedData$X_minus[is.na(cleanedData$X_minus)] = 0
cleanedData$Y_minus[is.na(cleanedData$Y_minus)] = 0

cleanedData$X_minus[!is.na(baseData$X_minus)] = - cleanedData$X_minus[!is.na(baseData$X_minus)] + 5
cleanedData$Y_minus[!is.na(baseData$Y_minus)] = - cleanedData$Y_minus[!is.na(baseData$Y_minus)] + 5

cleanedData$X = cleanedData$X + cleanedData$X_minus + (cleanedData$Column - 1) * 20 + ((as.numeric(cleanedData$SubQuad)-1) %/%4 ) * 5
cleanedData$Y = cleanedData$Y + cleanedData$Y_minus + (cleanedData$Row - 1) * 20 + (as.numeric(cleanedData$SubQuad) %%4) * 5

# Comment Wirong -I think we should subtract like the cleanedData$X. Because if trees are in subquadrat A4,B4,C4 or D4, it will shift to 0 instead of 15. There are some codes that I totally don't understand. I might ask you some days.

# FH: Unclear, check back

cleanedData$X_minus = NULL
cleanedData$Y_minus = NULL


#################
# DBH 

indices <- which(cleanedData$Dbh_a > 100)
cleanedData$removedInconsistencies[indices] <- paste(cleanedData$removedInconsistencies[indices], "DBH > 100", cleanedData$Dbh_a[indices])
cleanedData$Dbh_a[indices] = NA

indices <- which(cleanedData$Dbh_b > 100)
cleanedData$removedInconsistencies[indices] <- paste(cleanedData$removedInconsistencies[indices], "DBHb > 100", cleanedData$Dbh_b[indices])
cleanedData$Dbh_b[indices] = NA

indices <- which(cleanedData$Dbh_a < 4)
cleanedData$removedInconsistencies[indices] <- paste(cleanedData$removedInconsistencies[indices], "DBH < 4", cleanedData$Dbh_a[indices])
cleanedData$Dbh_a[indices] = NA

indices <- which(cleanedData$Dbh_b < 4)
cleanedData$removedInconsistencies[indices] <- paste(cleanedData$removedInconsistencies[indices], "DBHb < 4", cleanedData$Dbh_b[indices])
cleanedData$Dbh_b[indices] = NA


indices <- which(cleanedData$Dbh_a < cleanedData$Dbh_b)
cleanedData$removedInconsistencies[indices] <- paste(cleanedData$removedInconsistencies[indices], "DBHa < DBHb", cleanedData$Dbh_a[indices], cleanedData$Dbh_b[indices])
tmp <- cleanedData$Dbh_a[indices] 
cleanedData$Dbh_a[indices] <- cleanedData$Dbh_b[indices] 
cleanedData$Dbh_b[indices] <- tmp


############
# TH / H1st Branc

plot(cleanedData$TH,cleanedData$H_1stB, col = cleanedData$Plot)
abline(0,1)
legend("bottomright", legend = as.character(levels(cleanedData$Plot)), pch = 1, col = palette())

plot(cleanedData$Dbh_a, cleanedData$TH, xlim = c(0,50))
sel = which (cleanedData$TH<cleanedData$H_1stB)
points(cleanedData$Dbh_a[sel], cleanedData$TH[sel], col = "red")
points(cleanedData$Dbh_a[sel], cleanedData$H_1stB[sel], col = "green")

indices <- which(cleanedData$TH < cleanedData$H_1stB)
cleanedData$removedInconsistencies[indices] <- paste(cleanedData$removedInconsistencies[indices], "TH < H_1stB", cleanedData$TH[indices], cleanedData$H_1stB[indices])
tmp <- cleanedData$TH[indices]
cleanedData$TH[indices]<- cleanedData$H_1stB[indices]
cleanedData$H_1stB[indices] <- tmp

cleanedData$crownHeight = cleanedData$TH - cleanedData$H_1stB 
cleanedData$relativeCrownHeight = cleanedData$crownHeight / cleanedData$TH


#################
# CROWN POSITION

plot(cleanedData$Dbh_a, cleanedData$Width_X, col = cleanedData$Plot)
legend("topright", legend = as.character(levels(cleanedData$Plot)), pch = 1, col = palette())

plot(cleanedData$Dbh_a, cleanedData$Width_Y, col = cleanedData$Plot)
legend("topright", legend = as.character(levels(cleanedData$Plot)), pch = 1, col = palette())

plot(cleanedData$Width_X, cleanedData$Width_Y, col = cleanedData$Plot)
legend("topright", legend = as.character(levels(cleanedData$Plot)), pch = 1, col = palette())

plot(cleanedData$Dbh_a, cleanedData$Crown_Z1, col = cleanedData$Plot)
legend("topright", legend = as.character(levels(cleanedData$Plot)), pch = 1, col = palette())

plot(cleanedData$Dbh_a, cleanedData$Crown_Z2, col = cleanedData$Plot)
legend("topright", legend = as.character(levels(cleanedData$Plot)), pch = 1, col = palette())

plot(cleanedData$Dbh_a, cleanedData$Crown_X, col = cleanedData$Plot)
legend("topright", legend = as.character(levels(cleanedData$Plot)), pch = 1, col = palette())

plot(cleanedData$Dbh_a, cleanedData$Crown_Y, col = cleanedData$Plot)
legend("topright", legend = as.character(levels(cleanedData$Plot)), pch = 1, col = palette())


indices <- which(cleanedData$Width_Y > 2* cleanedData$Width_X)
cleanedData$removedInconsistencies[indices] <- paste(cleanedData$removedInconsistencies[indices], "WidthX > 2 * WidthY", cleanedData$Width_X[indices], cleanedData$Width_Y[indices])
cleanedData$Width_Y[indices] <- cleanedData$Width_X[indices]


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


#####################
# INCLINATION

cleanedData$inclination[heightMeasured] <- atan(sqrt((cleanedData$Crown_X[heightMeasured]-cleanedData$Trunk_X[heightMeasured])^2 
                                                     + (cleanedData$Crown_Y[heightMeasured] - cleanedData$Trunk_Y[heightMeasured])^2) / cleanedData$TH[heightMeasured])

plot(cleanedData$inclination, cleanedData$H_1stB/cleanedData$TH, col = cleanedData$Plot)
abline(1,0)

####################
# CROWN AREA

#crowncoordinates <- foreach(i=1:datarows) %do% get.crown.points(cleanedData[i,])
#cleanedData$crownArea <- unlist(foreach(i=1:datarows) %do% if (is.na(crowncoordinates[[i]])) NA else areapl(crowncoordinates[[i]]))

cleanedData$crownArea = pi * cleanedData$Width_X * cleanedData$Width_Y / 4

plot(cleanedData$Dbh_a, cleanedData$crownArea, col = cleanedData$Plot)

####################
# CROWN VOLUME

# shapefactor assumes that F (flat), O (oval) and S (Spere) are esentially 

shapefactor <- c(2/3, 2/3, 1/3, 2/3, NA)

cleanedData$crownVolume <- cleanedData$crownArea*cleanedData$crownHeigh* 
  shapefactor[ifelse(is.na(cleanedData$Shape), 5,as.numeric(cleanedData$Shape))]

plot(cleanedData$Dbh_a, cleanedData$crownVolume, col = cleanedData$Plot)


# SUMMARIZE INCONSISTENCIES

inconsistent <- (cleanedData[cleanedData$removedInconsistencies != "", ])
write.csv2(inconsistent, "../results/inconsistencies.csv")
head(cleanedData[cleanedData$removedInconsistencies!="",])



save.image(file = "../data/cleanedData.RData")











