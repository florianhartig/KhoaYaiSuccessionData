#' script with basic settings. Change rootdirectory to your project location
#'
#' @author Florian Hartig \url{http://florianhartig.wordpress.com/}


library(plyr)
library(boot)
library(lme4)
library(vegan)
library(foreach)
library(splancs)

rootdirectory <- "/Users/Florian/Home/Projekte/Papers_in_Progress/14-WirongCrownShape/KhoaYaiSuccessionData/"
setwd(rootdirectory)


dir.create(file.path(rootdirectory, "results/paper"), showWarnings = FALSE)



########################################
# helper functions


# from package gtools

quantcut <- function (x, q = seq(0, 1, by = 0.25), na.rm = TRUE, ...) 
{
  quant <- quantile(x, q, na.rm = na.rm)
  dups <- duplicated(quant)
  if (any(dups)) {
    flag <- x %in% unique(quant[dups])
    retval <- ifelse(flag, paste("[", as.character(x), "]", 
                                 sep = ""), NA)
    uniqs <- unique(quant)
    reposition <- function(cut) {
      flag <- x >= cut
      if (sum(flag) == 0) 
        return(cut)
      else return(min(x[flag], na.rm = na.rm))
    }
    newquant <- sapply(uniqs, reposition)
    retval[!flag] <- as.character(cut(x[!flag], breaks = newquant, 
                                      include.lowest = TRUE, ...))
    levs <- unique(retval[order(x)])
    retval <- factor(retval, levels = levs)
    mkpairs <- function(x) sapply(x, function(y) if (length(y) == 
                                                       2) 
      y[c(2, 2)]
      else y[2:3])
    pairs <- mkpairs(strsplit(levs, "[^0-9+\\.\\-]+"))
    rownames(pairs) <- c("lower.bound", "upper.bound")
    colnames(pairs) <- levs
    closed.lower <- rep(F, ncol(pairs))
    closed.upper <- rep(T, ncol(pairs))
    closed.lower[1] <- TRUE
    for (i in 2:ncol(pairs)) if (pairs[1, i] == pairs[1, 
                                                      i - 1] && pairs[1, i] == pairs[2, i - 1]) 
      closed.lower[i] <- FALSE
    for (i in 1:(ncol(pairs) - 1)) if (pairs[2, i] == pairs[1, 
                                                            i + 1] && pairs[2, i] == pairs[2, i + 1]) 
      closed.upper[i] <- FALSE
    levs <- ifelse(pairs[1, ] == pairs[2, ], pairs[1, ], 
                   paste(ifelse(closed.lower, "[", "("), pairs[1, ], 
                         ",", pairs[2, ], ifelse(closed.upper, "]", ")"), 
                         sep = ""))
    levels(retval) <- levs
  }
  else retval <- cut(x, quant, include.lowest = TRUE, ...)
  return(retval)
}


