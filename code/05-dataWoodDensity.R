

#######################################################
# mapping traits
#' Data preparation and cleaning wood density
#'
#' @author Florian Hartig \url{http://florianhartig.wordpress.com/}

woodDensity <- read.csv2("data/GlobalWoodDensityDatabase.csv")

colnames(woodDensity)[which(colnames(woodDensity) == "Wood.density..g.cm.3...oven.dry.mass.fresh.volume")] = "Density"

woodDensity <- ddply(woodDensity, .(Binomial), summarise,
             medianDensity = median(Density, na.rm = TRUE))


   
match <- function(speciesname){
  if (as.character(speciesname) %in% as.character(woodDensity$Binomial)) 
    return(which(woodDensity$Binomial == as.character(speciesname)))
  else return(NA)
}
  
out1<- sapply(cleanedData$Species, match)
  
    
getWoodDensitySpecies <- 
  else {
    index = agrep( as.character(speciesname), as.character(woodDensity$Binomial), max.distance = 2)
    if (index == T) return paste ("check", index)
  }  return 

%in% )
    
    
    
  
  return(woodDensity$Wood.density..g.cm.3...oven.dry.mass.fresh.volume)
}

cleanedData$woodDensity <- rep (NA, datarows)

agrep(as.character(cleanedData$Species[1000]), as.character(woodDensity$Binomial))

sum(!as.character(cleanedData$Species) %in% as.character(woodDensity$Binomial))


