#' Data preparation and cleaning
#'
#' @author Florian Hartig \url{http://florianhartig.wordpress.com/}


# reading new Plot data
baseData <-read.csv("data/CrHi_Map_MapOK_Spp.csv")

speciesData <- read.csv("data/Species_List.csv")


##################################################################
# Reordering, Calculations, Matching
# Copies the original dataframe, all changes are made in the new df

# copying data 
cleanedData = baseData

datarows = nrow(cleanedData)


############
# mapping species names to ID
getName <- function(index){
  return(paste(speciesData[index,1], speciesData[index,2]))
}

getName(103)
getName(104)

cleanedData$Species = as.factor(sapply(as.character(baseData$Species_ID), getName))
cleanedData$Species_ID = NULL

##############
# removing unneccesary columns and renaming 
cleanedData$Plot_ID = NULL # redundant with Abbrev
cleanedData$CrHi_ID = NULL

colnames(cleanedData)[which(colnames(cleanedData) == "Abbrev")] = "Plot"

cleanedData$Stage = as.factor(cleanedData$Stage)
levels(cleanedData$Stage) = c("Establishment", "Exclusion", "Old-Growth")


# Consitency checks

# check for inconsitency in the position coordinates
which(!is.na(cleanedData$X+cleanedData$X_minus))
which(!is.na(cleanedData$Y+cleanedData$Y_minus))

min(c(cleanedData$X,cleanedData$X_minus, cleanedData$Y,cleanedData$Y_minus), na.rm=T)
max(c(cleanedData$X,cleanedData$X_minus, cleanedData$Y,cleanedData$Y_minus), na.rm=T)

which(cleanedData$X > 5)
which(cleanedData$X_minus > 5)
which(cleanedData$Y > 5)
which(cleanedData$Y_minus > 5)

cleanedData$Dbh_a[which(cleanedData$Dbh_a > 100)]


plot(cleanedData$TH,cleanedData$H_1stB, col = cleanedData$Plot)
abline(0,1)
legend("bottomright", legend = as.character(levels(cleanedData$Plot)), pch = 1, col = palette())


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


#################
# dealing with wrong values

cleanedData$X[is.na(cleanedData$X)] = 0
cleanedData$Y[is.na(cleanedData$Y)] = 0
cleanedData$X_minus[is.na(cleanedData$X_minus)] = 0
cleanedData$Y_minus[is.na(cleanedData$Y_minus)] = 0

resampleColumn <- function(vectorX) return(sample(vectorX, datarows, replace = T))

# replacing unrealistic coordinates with random coordinates 
cleanedData$X = ifelse(cleanedData$X > 5, resampleColumn(cleanedData$X[cleanedData$X < 5]), cleanedData$X)
cleanedData$X_minus = ifelse(cleanedData$X_minus > 5, resampleColumn(cleanedData$X_minus[cleanedData$X_minus < 5]), cleanedData$X_minus)
cleanedData$Y = ifelse(cleanedData$Y > 5, resampleColumn(cleanedData$Y[cleanedData$Y < 5]), cleanedData$Y)
cleanedData$Y_minus = ifelse(cleanedData$Y_minus > 5, resampleColumn(cleanedData$Y_minus[cleanedData$Y_minus < 5]), cleanedData$Y_minus)

cleanedData$Dbh_a[cleanedData$Dbh_a > 150] = NA

##############
# calculations of absolute coordinates

cleanedData$X_minus = - cleanedData$X_minus + 5
cleanedData$Y_minus = - cleanedData$Y_minus + 5

cleanedData$X = cleanedData$X + cleanedData$X_minus + (cleanedData$Column-1) * 20 + (as.numeric(cleanedData$SubQuad) %/%4) * 5
cleanedData$Y = cleanedData$Y + cleanedData$Y_minus + (cleanedData$Row-1) * 20 + (as.numeric(cleanedData$SubQuad) %%4) * 5

cleanedData$X_minus = NULL
cleanedData$Y_minus = NULL

cleanedData$crownHeight = cleanedData$TH - cleanedData$H_1stB 
cleanedData$crownHeight[cleanedData$crownHeight<0] = NA
cleanedData$relativeCrownHeight = cleanedData$crownHeight / cleanedData$TH

plot(cleanedData$Dbh_a, cleanedData$crownHeight, col = cleanedData$Plot)
plot(cleanedData$Dbh_a, cleanedData$relativeCrownHeight, col = cleanedData$Plot)
boxplot(cleanedData$relativeCrownHeight ~ cleanedData$Shape + cleanedData$Plot, las = 2, col = rep(c(1,2,3,4,5),8))


# other stuff

cleanedData$Shape[cleanedData$Shape == ""] = NA
cleanedData$Shape = factor(cleanedData$Shape)


str(cleanedData)

